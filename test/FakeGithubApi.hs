{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module FakeGithubApi (Server, start, stop, clearOutput, getOutput, setTokenLifetime) where

import Universum

import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types (status200, status201, status400, status404, methodPost, methodGet)
import Data.Aeson (encode, object, (.=), Value)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)

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
          now <- getCurrentTime
          let expiresAt = addUTCTime (fromIntegral lifetimeSeconds) now
          addOutput server $ "Requested access token for installation " <> instId
          respond $ responseLBS status200 [("Content-Type", "application/json")]
            (encode $ object
              [ "token" .= ("mock-access-token" :: Text)
              , "expires_at" .= iso8601Show expiresAt
              , "installation_id" .= instId
              ])
        else respond $ responseLBS status400 [] "Bad Request"

handleCommitStatusRequest :: Server -> Text -> Text -> Text -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleCommitStatusRequest server owner repo commitSha req respond =
    if requestMethod req == methodPost
        then do
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
          statuses <- getStatuses server commitSha
          respond $ responseLBS status200 [("Content-Type", "application/json")] (encode statuses)
        else respond $ responseLBS status400 [] "Bad Request"

data Server = Server
  { tid :: ThreadId
  , output :: IORef [Text]
  , statuses :: IORef (Map Text [Value])  -- Map from commit SHA to list of status objects
  , tokenLifetimeSeconds :: IORef Int
  }

start :: Int -> IO Server
start port = do
  started <- newEmptyMVar
  output <- newIORef []
  statuses <- newIORef Map.empty
  tokenLifetimeSeconds <- newIORef 3600  -- Default: 1 hour
  let settings = Warp.setPort port $ Warp.setBeforeMainLoop (putMVar started ()) Warp.defaultSettings
  rec
    let server = Server {tid, output, statuses, tokenLifetimeSeconds}
    tid <- forkIO $ Warp.runSettings settings $ app server
  takeMVar started
  pure server

stop :: Server -> IO ()
stop (Server {tid}) = killThread tid

addOutput :: Server -> Text -> IO ()
addOutput (Server {output}) msg = modifyIORef output (msg :)

clearOutput :: Server -> IO ()
clearOutput (Server {output, statuses, tokenLifetimeSeconds}) = do
  writeIORef output []
  writeIORef statuses Map.empty
  writeIORef tokenLifetimeSeconds 3600  -- Reset to default

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
