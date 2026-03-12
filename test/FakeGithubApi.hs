{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module FakeGithubApi (Server, start, stop, clearOutput, getOutput, setTokenLifetime, setTokenExpirationOffset) where

import Universum

import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types (status200, status201, status400, status401, status404, methodPost, methodGet)
import Data.Aeson (encode, object, (.=), Value)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime, addUTCTime, UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.ByteString as BS

import Control.Concurrent (forkIO, ThreadId, killThread)

-- Mock handler function
app :: Server -> Application
app server req respond = do
    let path = pathInfo req
    case path of
        ["app", "installations", instId, "access_tokens"] ->
            handleAccessTokenRequest server instId req respond
        ["repos", owner, repo, "statuses", commitSha] ->
            handleCommitStatusRequest server owner repo commitSha req respond
        ["repos", owner, repo, "commits", commitSha, "statuses"] ->
            handleGetCommitStatuses server owner repo commitSha req respond
        _ -> respond $ responseLBS status404 [] "Not Found"

handleAccessTokenRequest :: Server -> Text -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleAccessTokenRequest server instId req respond =
    if requestMethod req == methodPost
        then do
          -- Read token lifetime from server state
          lifetimeSeconds <- readIORef server.tokenLifetimeSeconds
          offset <- readIORef server.tokenExpirationOffset
          now <- getCurrentTime
          let actualExpiry = addUTCTime (fromIntegral lifetimeSeconds) now
          let reportedExpiry = addUTCTime (fromIntegral offset) actualExpiry

          -- Issue unique token
          n <- atomicModifyIORef server.tokenCounter (\c -> (c + 1, c + 1))
          let tokenText = "mock-access-token-" <> show n

          -- Track the token with its actual expiry
          modifyIORef server.validTokens (Map.insert tokenText actualExpiry)

          addOutput server $ "Requested access token for installation " <> instId
          respond $ responseLBS status200 [("Content-Type", "application/json")]
            (encode $ object
              [ "token" .= tokenText
              , "expires_at" .= iso8601Show reportedExpiry
              , "installation_id" .= instId
              ])
        else respond $ responseLBS status400 [] "Bad Request"

-- Validate the Bearer token from the Authorization header.
-- Returns Nothing if valid, or a 401 response if invalid/expired.
validateToken :: Server -> Request -> IO (Maybe Response)
validateToken server req = do
  tokens <- readIORef server.validTokens
  -- If no tokens have been issued yet, skip validation (backwards compat)
  if Map.null tokens
    then pure Nothing
    else do
      now <- getCurrentTime
      let mAuth = fmap snd $ find (\(k, _) -> k == "Authorization") (requestHeaders req)
      case mAuth of
        Just authHeader
          | Just tokenBS <- BS.stripPrefix "Bearer " authHeader -> do
              let tokenText = decodeUtf8 tokenBS
              case Map.lookup tokenText tokens of
                Just expiry
                  | now < expiry -> pure Nothing  -- Valid
                  | otherwise -> pure $ Just $ responseLBS status401 [] "Token expired"
                Nothing -> pure $ Just $ responseLBS status401 [] "Unknown token"
          | otherwise -> pure $ Just $ responseLBS status401 [] "Invalid Authorization header"
        Nothing -> pure $ Just $ responseLBS status401 [] "Missing Authorization header"

handleCommitStatusRequest :: Server -> Text -> Text -> Text -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleCommitStatusRequest server owner repo commitSha req respond =
    if requestMethod req == methodPost
        then do
          mReject <- validateToken server req
          case mReject of
            Just rejection -> respond rejection
            Nothing -> do
              body <- strictRequestBody req
              -- Store the status for later retrieval
              storeStatus server commitSha body
              -- Note: commit SHA omitted because it's nondeterministic
              addOutput server $ "Updated commit status for " <> owner <> "/" <> repo <> " to " <> decodeUtf8 body
              respond $ responseLBS status201 [("Content-Type", "application/json")]
                (encode $ object ["state" .= ("success" :: Text), "sha" .= commitSha, "repository" .= repo, "owner" .= owner])
        else respond $ responseLBS status400 [] "Bad Request"

handleGetCommitStatuses :: Server -> Text -> Text -> Text -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGetCommitStatuses server _owner _repo commitSha req respond =
    if requestMethod req == methodGet
        then do
          mReject <- validateToken server req
          case mReject of
            Just rejection -> respond rejection
            Nothing -> do
              statuses <- getStatuses server commitSha
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode statuses)
        else respond $ responseLBS status400 [] "Bad Request"

data Server = Server
  { tid :: ThreadId
  , output :: IORef [Text]
  , statuses :: IORef (Map Text [Value])  -- Map from commit SHA to list of status objects
  , tokenLifetimeSeconds :: IORef Int
  , tokenCounter :: IORef Int
  , validTokens :: IORef (Map Text UTCTime)  -- Map from token to actual expiry time
  , tokenExpirationOffset :: IORef Int  -- Seconds to add to reported expires_at (simulates clock skew)
  }

start :: Int -> IO Server
start port = do
  started <- newEmptyMVar
  output <- newIORef []
  statuses <- newIORef Map.empty
  tokenLifetimeSeconds <- newIORef 3600  -- Default: 1 hour
  tokenCounter <- newIORef 0
  validTokens <- newIORef Map.empty
  tokenExpirationOffset <- newIORef 0
  let settings = Warp.setPort port $ Warp.setBeforeMainLoop (putMVar started ()) Warp.defaultSettings
  rec
    let server = Server {tid, output, statuses, tokenLifetimeSeconds, tokenCounter, validTokens, tokenExpirationOffset}
    tid <- forkIO $ Warp.runSettings settings $ app server
  takeMVar started
  pure server

stop :: Server -> IO ()
stop (Server {tid}) = killThread tid

addOutput :: Server -> Text -> IO ()
addOutput (Server {output}) msg = modifyIORef output (msg :)

clearOutput :: Server -> IO ()
clearOutput server = do
  writeIORef server.output []
  writeIORef server.statuses Map.empty
  writeIORef server.tokenLifetimeSeconds 3600  -- Reset to default
  writeIORef server.tokenCounter 0
  writeIORef server.validTokens Map.empty
  writeIORef server.tokenExpirationOffset 0

getOutput :: Server -> IO [Text]
getOutput (Server {output}) = reverse <$> readIORef output

storeStatus :: Server -> Text -> LByteString -> IO ()
storeStatus (Server {statuses}) commitSha statusBody = do
  let statusValue = case Aeson.eitherDecode statusBody of
        Left _ -> object []  -- Fallback to empty object if parse fails
        Right (v :: Value) -> v
  modifyIORef statuses $ Map.insertWith (<>) commitSha [statusValue]

getStatuses :: Server -> Text -> IO [Value]
getStatuses (Server {statuses}) commitSha = do
  statusMap <- readIORef statuses
  pure $ fromMaybe [] $ Map.lookup commitSha statusMap

setTokenLifetime :: Server -> Int -> IO ()
setTokenLifetime server seconds = writeIORef server.tokenLifetimeSeconds seconds

setTokenExpirationOffset :: Server -> Int -> IO ()
setTokenExpirationOffset server seconds = writeIORef server.tokenExpirationOffset seconds
