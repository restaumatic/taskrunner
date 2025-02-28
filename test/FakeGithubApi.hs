{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module FakeGithubApi (Server, start, stop, clearOutput, getOutput) where

import Universum

import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types (status200, status201, status400, status404, methodPost)
import Data.Aeson (encode, object, (.=))

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
        _ -> respond $ responseLBS status404 [] "Not Found"

handleAccessTokenRequest :: Server -> Text -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleAccessTokenRequest server instId req respond =
    if requestMethod req == methodPost
        then do
          addOutput server $ "Requested access token for installation " <> instId
          respond $ responseLBS status200 [("Content-Type", "application/json")]
            (encode $ object ["token" .= ("mock-access-token" :: Text), "installation_id" .= instId])
        else respond $ responseLBS status400 [] "Bad Request"

handleCommitStatusRequest :: Server -> Text -> Text -> Text -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleCommitStatusRequest server owner repo commitSha req respond =
    if requestMethod req == methodPost
        then do
          body <- strictRequestBody req
          -- Note: commit SHA omitted because it's nondeterministic
          addOutput server $ "Updated commit status for " <> owner <> "/" <> repo <> " to " <> decodeUtf8 body
          respond $ responseLBS status201 [("Content-Type", "application/json")]
            (encode $ object ["state" .= ("success" :: Text), "sha" .= commitSha, "repository" .= repo, "owner" .= owner])
        else respond $ responseLBS status400 [] "Bad Request"

data Server = Server
  { tid :: ThreadId
  , output :: IORef [Text]
  }

start :: Int -> IO Server
start port = mdo
  started <- newEmptyMVar
  output <- newIORef []
  let settings = Warp.setPort port $ Warp.setBeforeMainLoop (putMVar started ()) Warp.defaultSettings
  tid <- forkIO $ Warp.runSettings settings $ app server
  let server = Server {tid, output}
  takeMVar started
  pure server

stop :: Server -> IO ()
stop (Server {tid}) = killThread tid

addOutput :: Server -> Text -> IO ()
addOutput (Server {output}) msg = modifyIORef output (msg :)

clearOutput :: Server -> IO ()
clearOutput (Server {output}) = writeIORef output []

getOutput :: Server -> IO [Text]
getOutput (Server {output}) = reverse <$> readIORef output
