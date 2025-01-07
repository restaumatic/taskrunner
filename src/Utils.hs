module Utils where

import Universum

import qualified Data.ByteString.Char8 as B8
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Types
import Control.Exception (throwIO)
import Text.Printf (printf)
import Prelude (until)
import Data.List ((!!))
import System.Process (CreateProcess(..), StdStream (..), readCreateProcess)
import Data.Conduit.Process (proc)
import qualified Data.Text as Text
import GHC.IO.Handle (hDuplicate, hIsClosed)
import System.FilePath ((</>))

outputLine :: AppState -> Handle -> ByteString -> ByteString -> IO ()
outputLine appState toplevelOutput streamName line = do
    let jobName = B8.pack appState.jobName
    timestamp <- getCurrentTime
    let timestampStr
          | appState.settings.timestamps =
              -- TODO: add milliseconds somehow
              B8.pack (formatTime defaultTimeLocale "%T" timestamp) <> " "
          | otherwise = ""

    logClosed <- hIsClosed appState.logOutput
    unless logClosed do
      B8.hPutStrLn appState.logOutput $ timestampStr <> streamName <> " | " <> line

    let shouldOutputToToplevel
          | streamName == "debug" = appState.settings.logDebug
          | streamName == "info" = appState.settings.logInfo
          | otherwise = True

    when shouldOutputToToplevel do
      B8.hPutStrLn toplevelOutput $ timestampStr <> "[" <> jobName <> "] " <> streamName <> " | " <> line

logLevel :: MonadIO m => ByteString -> AppState -> Text -> m ()
logLevel level appState msg =
  liftIO $ forM_ (lines msg) $ outputLine appState appState.toplevelStderr level . encodeUtf8

logDebug :: MonadIO m => AppState -> Text -> m ()
logDebug = logLevel "debug"

logInfo :: MonadIO m => AppState -> Text -> m ()
logInfo = logLevel "info"

logError :: MonadIO m => AppState -> Text -> m ()
logError = logLevel "error"

logWarn :: MonadIO m => AppState -> Text -> m ()
logWarn = logLevel "warn"

newtype TaskrunnerError = TaskrunnerError String deriving newtype (Show)

instance Exception TaskrunnerError

-- TODO: get rid of this
bail :: String -> IO a
bail s = throwIO $ TaskrunnerError s

-- | Given a printf format string for the decimal part and a number of
-- bytes, formats the bytes using an appropriate unit and returns the
-- formatted string.
--
-- >>> bytesfmt "%.2" 512368
-- "500.359375 KiB"
bytesfmt :: Integral a => String -> a -> String
bytesfmt formatter bs = printf (formatter <> " %s")
                               (fromIntegral (signum bs) * dec :: Double)
                               bytesSuffix
 where
  (dec, i) = getSuffix (abs bs)
  getSuffix n = until p (\(x, y) -> (x / 1024, y + 1)) (fromIntegral n, 0)
   where
    p (n', numDivs) = n' < 1024 || numDivs == length bytesSuffixes - 1
  bytesSuffixes :: [String]
  bytesSuffixes = ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]
  bytesSuffix = bytesSuffixes !! i

getCurrentBranch :: AppState -> IO Text
getCurrentBranch appState =
  bracket (hDuplicate appState.subprocessStderr) hClose \stderr_ ->
    Text.strip . Text.pack <$> readCreateProcess
      (proc "git" ["symbolic-ref", "--short", "HEAD"]) { std_err = UseHandle stderr_ }
       ""

getMainBranchCommit :: AppState -> IO (Maybe Text)
getMainBranchCommit appState =
  case appState.settings.mainBranch of
    Nothing ->
      pure Nothing
    Just branch ->
      bracket (hDuplicate appState.subprocessStderr) hClose \stderr_ ->
        Just . Text.strip . Text.pack <$> readCreateProcess
          (proc "git" ["merge-base", "HEAD", "origin/" <> toString branch]) { std_err = UseHandle stderr_ }
           ""

getCurrentCommit :: AppState -> IO Text
getCurrentCommit _appState =
  -- TODO: fix: we can't use subprocessStderr here because it's used after closing output collector
  -- Using normal stderr for now
--  bracket (hDuplicate appState.subprocessStderr) hClose \stderr_ ->
    Text.strip . Text.pack <$> readCreateProcess
      (proc "git" ["rev-parse", "HEAD"])
       ""

logFileName :: Settings -> BuildId -> JobName -> FilePath
logFileName settings buildId jobName = settings.stateDirectory </> "builds" </> toString buildId </> "logs" </> (jobName <> ".log")
