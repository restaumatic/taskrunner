{-# LANGUAGE OverloadedStrings #-}

module CommitStatus where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..), encode, eitherDecodeFileStrict)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Web.JWT (Algorithm(RS256), JWTClaimsSet(..), encodeSigned, numericDate, stringOrURI, EncodeSigner (..), readRsaSecret, JOSEHeader (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv, lookupEnv)
import Network.HTTP.Types.Status (Status(..))
import Data.Aeson.Decoding (eitherDecode)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BL
import System.FileLock (withFileLock, SharedExclusive(..))
import System.Directory (doesFileExist)
import Utils (getCurrentCommit, logError, logDebug)
import Types (AppState(..), GithubClient(..), Settings(..))

-- Define the data types for the status update
data StatusRequest = StatusRequest
  { state       :: T.Text
  , target_url  :: Maybe T.Text
  , description :: Maybe T.Text
  , context     :: T.Text
  } deriving (Show, Generic)
  deriving anyclass (ToJSON)

-- Define the data type for parsing status responses
data StatusResponse = StatusResponse
  { context :: T.Text
  , state   :: T.Text
  } deriving (Show, Generic)
  deriving anyclass (FromJSON)

-- Define the data type for the installation token response
data InstallationTokenResponse = InstallationTokenResponse
  { token :: T.Text
  , expires_at :: T.Text
  } deriving (Show, Generic)
  deriving anyclass (FromJSON)

-- Cache file for storing credentials across processes
data CredentialsCache = CredentialsCache
  { cachedToken :: T.Text
  , cachedExpiresAt :: T.Text
  } deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

credentialsCacheFile :: Settings -> FilePath
credentialsCacheFile settings = settings.stateDirectory <> "/.github-token-cache.json"

-- Try to read cache file (no locking - caller should hold lock)
tryReadCache :: FilePath -> IO (Maybe (T.Text, UTCTime))
tryReadCache cacheFile = do
  exists <- doesFileExist cacheFile
  if exists then do
    result <- eitherDecodeFileStrict @CredentialsCache cacheFile
    case result of
      Left _ -> pure Nothing
      Right cache ->
        case iso8601ParseM (toString cache.cachedExpiresAt) of
          Just expiresAt -> pure $ Just (cache.cachedToken, expiresAt)
          Nothing -> pure Nothing
  else
    pure Nothing

getClient :: AppState -> IO GithubClient
getClient appState = do
  mClient <- readIORef appState.githubClient
  case mClient of
    Just client -> do
      -- Fast path: check if cached token is still valid
      now <- getCurrentTime
      if diffUTCTime client.expiresAt now >= 300
        then pure client
        else do
          -- Token expiring, need to refresh
          logDebug appState $ "GitHub token expired or expiring soon (in " <> show (floor (diffUTCTime client.expiresAt now) :: Int) <> "s), refreshing..."
          writeIORef appState.githubClient Nothing
          loadOrRefreshClient appState
    Nothing ->
      loadOrRefreshClient appState

loadOrRefreshClient :: AppState -> IO GithubClient
loadOrRefreshClient appState = do
  let cacheFile = credentialsCacheFile appState.settings
  let lockFile = cacheFile <> ".lock"

  client <- withFileLock lockFile Exclusive \_ -> do
    -- Under EXCLUSIVE lock: read, check, refresh if needed
    mCached <- tryReadCache cacheFile

    now <- getCurrentTime
    case mCached of
      Just (cachedToken, expiresAt)
        | diffUTCTime expiresAt now >= 300 -> do
            -- Valid cached token
            logDebug appState "Using cached GitHub token from file"
            buildClientWithToken appState cachedToken expiresAt
        | otherwise -> do
            -- Expired token, refresh
            logDebug appState "Cached token expired, refreshing"
            refreshToken appState cacheFile
      Nothing -> do
        -- No cache, create new token
        logDebug appState "No cached token, creating new one"
        refreshToken appState cacheFile

  writeIORef appState.githubClient (Just client)
  pure client

-- Create new token and write to cache (caller should hold EXCLUSIVE lock)
refreshToken :: AppState -> FilePath -> IO GithubClient
refreshToken appState cacheFile = do
  tokenResponse <- createTokenFromGitHub appState

  expiresAt <- case iso8601ParseM (toString tokenResponse.expires_at) of
    Just t -> pure t
    Nothing -> do
      logError appState $ "CommitStatus: Failed to parse expires_at: " <> tokenResponse.expires_at
      exitFailure

  -- Write to cache (already under EXCLUSIVE lock, no additional locking needed)
  let cache = CredentialsCache
        { cachedToken = tokenResponse.token
        , cachedExpiresAt = T.pack $ iso8601Show expiresAt
        }
  BL.writeFile cacheFile (encode cache)

  -- Build and return client
  buildClientWithToken appState tokenResponse.token expiresAt

buildClientWithToken :: AppState -> T.Text -> UTCTime -> IO GithubClient
buildClientWithToken _appState accessToken expiresAt = do
  -- Load environment variables
  apiUrl <- fromMaybe "https://api.github.com" <$> lookupEnv "GITHUB_API_URL"
  appId <- getEnv "GITHUB_APP_ID"
  installationId <- getEnv "GITHUB_INSTALLATION_ID"
  privateKeyStr <- getEnv "GITHUB_APP_PRIVATE_KEY"
  owner <- getEnv "GITHUB_REPOSITORY_OWNER"
  repo <- getEnv "GITHUB_REPOSITORY"
  manager <- HTTP.newManager tlsManagerSettings

  pure $ GithubClient
    { apiUrl = T.pack apiUrl
    , appId = T.pack appId
    , installationId = T.pack installationId
    , privateKey = T.pack privateKeyStr
    , owner = T.pack owner
    , repo = T.pack repo
    , manager = manager
    , accessToken = accessToken
    , expiresAt = expiresAt
    }

-- Create a new GitHub App installation token from GitHub API
createTokenFromGitHub :: AppState -> IO InstallationTokenResponse
createTokenFromGitHub appState = do
  -- Load environment variables
  apiUrl <- fromMaybe "https://api.github.com" <$> lookupEnv "GITHUB_API_URL"
  appId <- getEnv "GITHUB_APP_ID"
  installationId <- getEnv "GITHUB_INSTALLATION_ID"
  privateKeyStr <- getEnv "GITHUB_APP_PRIVATE_KEY"

  -- Prepare the HTTP manager
  manager <- HTTP.newManager tlsManagerSettings

  let privateKeyBytes = encodeUtf8 $ Text.replace "|" "\n" $ toText privateKeyStr
  let privateKey = fromMaybe (error "Invalid github key") $ readRsaSecret privateKeyBytes

  -- Create the JWT token
  now <- getPOSIXTime
  let claims = mempty { iss = stringOrURI $ T.pack appId
                   , iat = numericDate now
                   , exp = numericDate (now + 5 * 60)
                   }
  let jwt = encodeSigned (EncodeRSAPrivateKey privateKey) (mempty { alg = Just RS256 }) claims

  -- Get the installation access token
  let installUrl = apiUrl <> "/app/installations/" ++ installationId ++ "/access_tokens"
  initRequest <- HTTP.parseRequest installUrl
  let request = initRequest
                { HTTP.method = "POST"
                , HTTP.requestHeaders =
                    [ ("Authorization", "Bearer " <> TE.encodeUtf8 jwt)
                    , ("Accept", "application/vnd.github.v3+json")
                    , ("User-Agent", "restaumatic-bot")
                    ]
                }
  response <- HTTP.httpLbs request manager
  let mTokenResponse = eitherDecode @InstallationTokenResponse (HTTP.responseBody response)
  case mTokenResponse of
    Left err -> do
      logError appState $ "CommitStatus: Failed to parse installation token response: " <> show err
      logError appState $ "CommitStatus: Response: " <> decodeUtf8 response.responseBody
      -- FIXME: handle the error better
      exitFailure
    Right tokenResponse ->
      pure tokenResponse

updateCommitStatus :: MonadIO m => AppState -> StatusRequest -> m ()
updateCommitStatus appState statusRequest = liftIO do
  client <- getClient appState
  sha <- getCurrentCommit appState

  -- Prepare the status update request
  let statusUrl = toString client.apiUrl <> "/repos/" ++ toString client.owner ++ "/" ++ toString client.repo ++ "/statuses/" ++ toString sha
  initStatusRequest <- HTTP.parseRequest statusUrl
  let statusReq = initStatusRequest
                  { HTTP.method = "POST"
                  , HTTP.requestHeaders =
                      [ ("Authorization", "Bearer " <> TE.encodeUtf8 client.accessToken)
                      , ("Accept", "application/vnd.github.v3+json")
                      , ("Content-Type", "application/json")
                      , ("User-Agent", "restaumatic-bot")
                      ]
                  , HTTP.requestBody = HTTP.RequestBodyLBS $ encode statusRequest
                  }
  statusResponse <- HTTP.httpLbs statusReq client.manager
  if statusResponse.responseStatus.statusCode == 201
    then
      logDebug appState "Commit status updated successfully"
    else do
      logError appState $ "CommitStatus: Failed to update commit status: " <> show statusResponse
      logError appState $ "CommitStatus: Response: " <> decodeUtf8 statusResponse.responseBody
      exitFailure

-- Check if a status exists for the current commit and context
checkExistingStatus :: MonadIO m => AppState -> T.Text -> m Bool
checkExistingStatus appState contextName = liftIO do
  client <- getClient appState
  sha <- getCurrentCommit appState

  -- Prepare the GET request for statuses
  let statusUrl = toString client.apiUrl <> "/repos/" ++ toString client.owner ++ "/" ++ toString client.repo ++ "/commits/" ++ toString sha ++ "/statuses"
  initStatusRequest <- HTTP.parseRequest statusUrl
  let statusReq = initStatusRequest
                  { HTTP.method = "GET"
                  , HTTP.requestHeaders =
                      [ ("Authorization", "Bearer " <> TE.encodeUtf8 client.accessToken)
                      , ("Accept", "application/vnd.github.v3+json")
                      , ("User-Agent", "restaumatic-bot")
                      ]
                  }
  statusResponse <- HTTP.httpLbs statusReq client.manager
  if statusResponse.responseStatus.statusCode == 200
    then do
      let mStatuses = eitherDecode @[StatusResponse] (HTTP.responseBody statusResponse)
      case mStatuses of
        Left err -> do
          logDebug appState $ "CommitStatus: Failed to parse statuses response: " <> show err
          pure False
        Right statuses -> do
          let hasContext = any (\s -> s.context == contextName) statuses
          logDebug appState $ "CommitStatus: Found existing status for context " <> contextName <> ": " <> show hasContext
          pure hasContext
    else do
      logDebug appState $ "CommitStatus: Failed to get commit statuses: " <> show statusResponse.responseStatus.statusCode
      pure False
