{-# LANGUAGE OverloadedStrings #-}

module CommitStatus where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..), encode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Web.JWT (Algorithm(RS256), JWTClaimsSet(..), encodeSigned, numericDate, stringOrURI, EncodeSigner (..), readRsaSecret, JOSEHeader (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv, lookupEnv)
import Network.HTTP.Types.Status (Status(..))
import Data.Aeson.Decoding (eitherDecode)
import qualified Data.Text as Text
import Utils (getCurrentCommit, logError, logDebug)
import Types (AppState(..), GithubClient(..))

-- Define the data types for the status update
data StatusRequest = StatusRequest
  { state       :: T.Text
  , target_url  :: Maybe T.Text
  , description :: Maybe T.Text
  , context     :: T.Text
  } deriving (Show, Generic)
  deriving anyclass (ToJSON)

-- Define the data type for the installation token response
newtype InstallationTokenResponse = InstallationTokenResponse
  { token :: T.Text
  } deriving (Show, Generic)
  deriving anyclass (FromJSON)

getClient :: AppState -> IO GithubClient
getClient appState = do
  mClient <- readIORef appState.githubClient
  case mClient of
    Just client -> pure client
    Nothing -> do
      client <- initClient appState
      writeIORef appState.githubClient $ Just client
      pure client

initClient :: AppState -> IO GithubClient
initClient appState = do
  -- Load environment variables
  apiUrl <- fromMaybe "https://api.github.com" <$> lookupEnv "GITHUB_API_URL"
  appId <- getEnv "GITHUB_APP_ID"
  installationId <- getEnv "GITHUB_INSTALLATION_ID"
  privateKeyStr <- getEnv "GITHUB_APP_PRIVATE_KEY"
  owner <- getEnv "GITHUB_REPOSITORY_OWNER"
  repo <- getEnv "GITHUB_REPOSITORY"
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
  accessToken <- case mTokenResponse of
    Left err -> do
      logError appState $ "CommitStatus: Failed to parse installation token response: " <> show err
      logError appState $ "CommitStatus: Response: " <> decodeUtf8 response.responseBody

      -- FIXME: handle the error better
      exitFailure
    Right tokenResponse ->
      pure tokenResponse.token


  pure $ GithubClient { apiUrl = T.pack apiUrl
                      , appId = T.pack appId
                      , installationId = T.pack installationId
                      , privateKey = T.pack privateKeyStr
                      , owner = T.pack owner
                      , repo = T.pack repo
                      , manager = manager
                      , accessToken = accessToken
                      }

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
