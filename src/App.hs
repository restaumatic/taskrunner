{-# LANGUAGE TemplateHaskell #-}

module App where

import Universum

import System.Environment (setEnv, lookupEnv, getEnvironment)
import System.Process (createProcess, CreateProcess (..), StdStream (CreatePipe, UseHandle), proc, waitForProcess, createPipe)
import System.IO (openBinaryFile, hSetBuffering, BufferMode (LineBuffering))
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent.Async (async, wait, cancel)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Control.Exception.Base (handle, throwIO)
import System.IO.Error (isEOFError, IOError)
import System.Posix.ByteString (stdOutput, fdToHandle, handleToFd)
import System.Posix (Fd, dup, stdError)
import CliArgs
import System.FileLock (withFileLock, SharedExclusive (Exclusive))
import Data.FileEmbed (embedStringFile)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import SnapshotCliArgs (SnapshotCliArgs)
import SnapshotCliArgs qualified 

data Settings = Settings
  { stateDirectory :: FilePath
  , timestamps :: Bool
  }

getSettings :: IO Settings
getSettings = do
  stateDirectory <- fromMaybe "/tmp/taskrunner" <$> lookupEnv "TASKRUNNER_STATE_DIRECTORY"
  timestamps <- (/=Just "1") <$> lookupEnv "TASKRUNNER_DISABLE_TIMESTAMPS"
  pure Settings
        { stateDirectory
        , timestamps
        }

main :: IO ()
main = do
  args <- getCliArgs
  settings <- getSettings

  -- TODO: better inference
  let jobName = fromMaybe (FilePath.takeFileName args.cmd) args.name

  let lockFileName = settings.stateDirectory </> "locks" </> (jobName <> ".lock")
  let logFileName = settings.stateDirectory </> "logs" </> (jobName <> ".log")

  createDirectoryIfMissing True (settings.stateDirectory </> "locks")

  withFileLock lockFileName Exclusive \_ -> do
    createDirectoryIfMissing True (settings.stateDirectory </> "logs")

    -- Lock (take) it while writing a line to either `logFile` or stdout
    logFile <- openBinaryFile logFileName WriteMode
    hSetBuffering logFile LineBuffering

    devnull <- openBinaryFile "/dev/null" WriteMode

    toplevelStdout <- toplevelStream "_taskrunner_toplevel_stdout" stdOutput
    toplevelStderr <- toplevelStream "_taskrunner_toplevel_stderr" stdError

    (requestPipeRead, requestPipeWrite) <- createPipe
    requestPipeWriteFd <- handleToFd requestPipeWrite
    (responsePipeRead, responsePipeWrite) <- createPipe
    responsePipeReadFd <- handleToFd responsePipeRead
    hSetBuffering responsePipeWrite LineBuffering

    parentEnv <- getEnvironment

    cmdHandler <- async $ commandHandler settings jobName requestPipeRead responsePipeWrite

    -- TODO: handle spawn error here
    -- TODO: should we use withCreateProcess?
    -- TODO: should we use delegate_ctlc or DIY? See https://hackage.haskell.org/package/process-1.6.20.0/docs/System-Process.html#g:4
    -- -> We should DIY because we need to flush stream etc.
    (_, Just stdoutPipe, Just stderrPipe, processHandle) <- createProcess
      (proc args.cmd args.args) { std_in = UseHandle devnull, std_out = CreatePipe, std_err = CreatePipe,
        env=Just $
          [ ("BASH_FUNC_snapshot%%", "() {\n" <> $(embedStringFile "src/snapshot.sh") <> "\n}")
          , ("_taskrunner_request_pipe", show requestPipeWriteFd)
          , ("_taskrunner_response_pipe", show responsePipeReadFd)
          ] <> parentEnv
        }

    stdoutHandler <- async $ outputStreamHandler settings (B8.pack jobName) logFile toplevelStdout "stdout" stdoutPipe
    stderrHandler <- async $ outputStreamHandler settings (B8.pack jobName) logFile toplevelStderr "stderr" stderrPipe

    exitCode <- waitForProcess processHandle

    -- TODO: timeout here
    wait stdoutHandler
    wait stderrHandler
    cancel cmdHandler

    exitWith exitCode

toplevelStream :: String -> Fd -> IO Handle
toplevelStream envName fd = do
  envValue <- lookupEnv envName
  newFd <- case envValue of
    Nothing -> do
      -- We are the top level.
      newFd <- dup fd
      setEnv envName (show newFd)
      pure newFd
    Just value ->
      case readMaybe value of
        Just x ->
          pure x
        Nothing ->
          bail $ "Invalid file descriptor " <> show value <> " in " <> envName
  h <- fdToHandle newFd
  hSetBuffering h LineBuffering
  pure h

outputStreamHandler :: Settings -> ByteString -> Handle -> Handle -> ByteString -> Handle -> IO ()
outputStreamHandler settings jobName logFile toplevelOutput streamName stream =
  handle ignoreEOF $ forever do
    line <- B8.hGetLine stream
    timestamp <- getCurrentTime
    let timestampStr
          | settings.timestamps =
              -- TODO: add milliseconds somehow
              B8.pack (formatTime defaultTimeLocale "%T" timestamp) <> " "
          | otherwise = ""

    B8.hPutStrLn logFile $ timestampStr <> streamName <> " | " <> line

    -- FIXME: since toplevelOutput is shared between multiple processes, the mutex makes little sense...
    -- We should probably just rely on atomicity of writes (and hope LineBuffering always works as expected)
    B8.hPutStrLn toplevelOutput $ timestampStr <> "[" <> jobName <> "] " <> streamName <> " | " <> line

commandHandler :: Settings -> String -> Handle -> Handle -> IO ()
commandHandler settings jobName requestPipe responsePipe =
  handle ignoreEOF $ forever do
    requestLine <- B8.hGetLine requestPipe
    case Aeson.eitherDecode @[String] (BL.fromStrict requestLine) of
      Left err -> do
        putStrLn @Text $ "Invalid command pipe request: " <> show requestLine <> "\nError: " <> show err
        B8.hPutStrLn responsePipe "exit 1"
      Right cmd -> do
        case SnapshotCliArgs.parse cmd of
          Left err -> do
            putStrLn @Text $ "snapshot: " <> toText err
            B8.hPutStrLn responsePipe "exit 1"
          Right args -> do
            response <- snapshot args
            B8.hPutStrLn responsePipe (encodeUtf8 response)

snapshot :: SnapshotCliArgs -> IO String
snapshot _args = do
  pure "true"

ignoreEOF :: IOError -> IO ()
ignoreEOF e | isEOFError e = pure ()
            | otherwise    = throwIO e

newtype TaskrunnerError = TaskrunnerError String deriving newtype (Show)

instance Exception TaskrunnerError

bail :: String -> IO a
bail s = throwIO $ TaskrunnerError s
