module Utils where

import Universum

import qualified Data.ByteString.Char8 as B8
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Types
import Control.Exception (throwIO)
import Text.Printf (printf)
import Prelude (until)
import Data.List ((!!))

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
