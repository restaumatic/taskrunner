module Utils where

import Universum

import qualified Data.ByteString.Char8 as B8
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Types
import Control.Exception (throwIO)

outputLine :: AppState -> Handle -> ByteString -> ByteString -> IO ()
outputLine appState toplevelOutput streamName line = do
    let jobName = B8.pack appState.jobName
    timestamp <- getCurrentTime
    let timestampStr
          | appState.settings.timestamps =
              -- TODO: add milliseconds somehow
              B8.pack (formatTime defaultTimeLocale "%T" timestamp) <> " "
          | otherwise = ""

    B8.hPutStrLn appState.logOutput $ timestampStr <> streamName <> " | " <> line
    B8.hPutStrLn toplevelOutput $ timestampStr <> "[" <> jobName <> "] " <> streamName <> " | " <> line

logLevel :: ByteString -> AppState -> Text -> IO ()
logLevel level appState msg =
  forM_ (lines msg) $ outputLine appState appState.toplevelStderr level . encodeUtf8

logDebug :: AppState -> Text -> IO ()
logDebug appState msg = when appState.settings.debug $ logLevel "debug" appState msg

logError :: AppState -> Text -> IO ()
logError = logLevel "error"

newtype TaskrunnerError = TaskrunnerError String deriving newtype (Show)

instance Exception TaskrunnerError

-- TODO: get rid of this
bail :: String -> IO a
bail s = throwIO $ TaskrunnerError s
