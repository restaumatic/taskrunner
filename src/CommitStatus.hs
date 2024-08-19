{-# LANGUAGE OverloadedStrings #-}

module CommitStatus where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..), encode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Web.JWT (Algorithm(RS256), JWTClaimsSet(..), encodeSigned, numericDate, stringOrURI, EncodeSigner (..), readRsaSecret, JOSEHeader (..))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)
import Network.HTTP.Types.Status (Status(..))
import Data.Aeson.Decoding (eitherDecode)
import qualified Data.Text as Text
import Utils (getCurrentCommit)
import Types (AppState)

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

updateCommitStatus :: AppState -> StatusRequest -> IO ()
updateCommitStatus appState statusRequest = do
  -- Load environment variables
  appId <- getEnv "GITHUB_APP_ID"
  installationId <- getEnv "GITHUB_INSTALLATION_ID"
  privateKeyStr <- getEnv "GITHUB_APP_PRIVATE_KEY"
  owner <- getEnv "GITHUB_REPOSITORY_OWNER"
  repo <- getEnv "GITHUB_REPOSITORY"

  sha <- getCurrentCommit appState

  let privateKeyBytes = encodeUtf8 $ Text.replace "|" "\n" $ toText privateKeyStr
  let privateKey = fromMaybe (error "Invalid github key") $ readRsaSecret privateKeyBytes

  -- Create the JWT token
  now <- getPOSIXTime
  let claims = mempty { iss = stringOrURI $ T.pack appId
                   , iat = numericDate now
                   , exp = numericDate (now + 10 * 60)
                   }
  let jwt = encodeSigned (EncodeRSAPrivateKey privateKey) (mempty { alg = Just RS256 }) claims

  -- Prepare the HTTP manager
  manager <- HTTP.newManager tlsManagerSettings

  -- Get the installation access token
  let installUrl = "https://api.github.com/app/installations/" ++ installationId ++ "/access_tokens"
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
      putStrLn @Text $ "Failed to parse installation token response: " <> show err
      putStrLn @Text $ "Response: " <> decodeUtf8 response.responseBody
    Right tokenResponse -> do
      let accessToken = token tokenResponse

      -- Prepare the status update request
      let statusUrl = "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/statuses/" ++ toString sha
      initStatusRequest <- HTTP.parseRequest statusUrl
      let statusReq = initStatusRequest
                      { HTTP.method = "POST"
                      , HTTP.requestHeaders =
                          [ ("Authorization", "Bearer " <> TE.encodeUtf8 accessToken)
                          , ("Accept", "application/vnd.github.v3+json")
                          , ("Content-Type", "application/json")
                          , ("User-Agent", "restaumatic-bot")
                          ]
                      , HTTP.requestBody = HTTP.RequestBodyLBS $ encode statusRequest
                      }
      statusResponse <- HTTP.httpLbs statusReq manager
      if statusResponse.responseStatus.statusCode == 201
        then putStrLn @Text "Status updated successfully!"
        else do
          putStrLn @Text "Failed to update status"
          BL.putStrLn $ HTTP.responseBody statusResponse

