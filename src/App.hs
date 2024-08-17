{-# LANGUAGE TemplateHaskell #-}

module App where

import Universum

import System.Environment (setEnv, lookupEnv, getEnvironment)
import System.Process (createProcess_, CreateProcess (..), StdStream (CreatePipe, UseHandle), proc, waitForProcess, createPipe,  readCreateProcess)
import System.IO (openBinaryFile, hSetBuffering, BufferMode (LineBuffering) )
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent.Async (async, wait, cancel)
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
import Data.Text qualified as Text
import qualified Data.Text.IO as Text
import GHC.IO.Exception (ExitCode(..))
import Crypto.Hash qualified as H
import Data.Containers.ListUtils (nubOrdOn)
import GHC.IO.Handle (hDuplicate)
import System.Timeout (timeout)
import Prelude (read)
import Types
import Utils
import qualified RemoteCache
import RemoteCache (getLatestBuildHash)

getSettings :: IO Settings
getSettings = do
  stateDirectory <- fromMaybe "/tmp/taskrunner" <$> lookupEnv "TASKRUNNER_STATE_DIRECTORY"
  cwd <- getCurrentDirectory
  rootDirectory <- fromMaybe cwd <$> lookupEnv "TASKRUNNER_ROOT_DIRECTORY"
  timestamps <- (/=Just "1") <$> lookupEnv "TASKRUNNER_DISABLE_TIMESTAMPS"
  debug <- (==Just "1") <$> lookupEnv "TASKRUNNER_DEBUG"
  outputStreamTimeout <- maybe 5 read <$> lookupEnv "TASKRUNNER_OUTPUT_STREAM_TIMEOUT"
  saveRemoteCache <- (==Just "1") <$> lookupEnv "TASKRUNNER_SAVE_REMOTE_CACHE"
  pure Settings
        { stateDirectory
        , rootDirectory
        , timestamps
        , debug
        , outputStreamTimeout
        , saveRemoteCache
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
  createDirectoryIfMissing True (settings.stateDirectory </> "hash")

  withFileLock lockFileName Exclusive \_ -> do
    createDirectoryIfMissing True (settings.stateDirectory </> "logs")

    -- Lock (take) it while writing a line to either `logFile` or stdout
    logFile <- openBinaryFile logFileName WriteMode
    hSetBuffering logFile LineBuffering

    devnull <- openBinaryFile "/dev/null" ReadMode

    toplevelStdout <- toplevelStream "_taskrunner_toplevel_stdout" stdOutput
    toplevelStderr <- toplevelStream "_taskrunner_toplevel_stderr" stdError

    (requestPipeRead, requestPipeWrite) <- createPipe
    requestPipeWriteFd <- handleToFd requestPipeWrite
    (responsePipeRead, responsePipeWrite) <- createPipe
    responsePipeReadFd <- handleToFd responsePipeRead
    hSetBuffering responsePipeWrite LineBuffering

    parentEnv <- getEnvironment
    (stderrPipe, subprocessStderr) <- createPipe

    appState <- AppState settings jobName <$> newIORef Nothing <*> newIORef Nothing <*> pure toplevelStderr <*> pure subprocessStderr <*> pure logFile

    cmdHandler <- async $ commandHandler appState requestPipeRead responsePipeWrite

    cwd <- getCurrentDirectory

    logDebug appState $ "Running command: " <> show (args.cmd : args.args)
    logDebug appState $ "  cwd: " <> show cwd
    logDebug appState $ "  settings: " <> show settings

    -- TODO: handle spawn error here
    -- TODO: should we use withCreateProcess?
    -- TODO: should we use delegate_ctlc or DIY? See https://hackage.haskell.org/package/process-1.6.20.0/docs/System-Process.html#g:4
    -- -> We should DIY because we need to flush stream etc.
    (Nothing, Just stdoutPipe, Nothing, processHandle) <- createProcess_ "createProcess_"
      (proc args.cmd args.args) { std_in = UseHandle devnull, std_out = CreatePipe, std_err = UseHandle subprocessStderr,
        env=Just $ nubOrdOn fst $
          [ ("BASH_FUNC_snapshot%%", "() {\n" <> $(embedStringFile "src/snapshot.sh") <> "\n}")
          , ("_taskrunner_request_pipe", show requestPipeWriteFd)
          , ("_taskrunner_response_pipe", show responsePipeReadFd)
          ] <> parentEnv
        }

    stdoutHandler <- async $ outputStreamHandler appState toplevelStdout "stdout" stdoutPipe
    stderrHandler <- async $ outputStreamHandler appState toplevelStderr "stderr" stderrPipe

    exitCode <- waitForProcess processHandle

    logDebug appState $ "Command " <> show (args.cmd : args.args) <> " exited with code " <> show exitCode

    when (exitCode == ExitSuccess) do
      whenJustM (readIORef appState.hashToSaveRef) \h -> do
        logDebug appState $ "Saving hash " <> h.hash <> " to " <> toText (hashFilename appState)
        Text.writeFile (hashFilename appState) (h.hash <> "\n\n" <> h.hashInput)

        whenJustM (readIORef appState.snapshotArgsRef) \snapshotArgs ->
          when (hasOutputs snapshotArgs && settings.saveRemoteCache) do
            logDebug appState "Saving remote cache"
            s <- RemoteCache.getRemoteCacheSettingsFromEnv
            RemoteCache.saveCache appState s (fromMaybe "." snapshotArgs.cacheRoot) snapshotArgs.outputs (archiveName appState snapshotArgs h.hash)

            when snapshotArgs.fuzzyCache do
              branch <- getCurrentBranch appState
              RemoteCache.setLatestBuildHash appState s (toText appState.jobName) branch h.hash

    timeoutStream appState "stdout" $ wait stdoutHandler

    -- We used `createProcess_`, so we must close it manually
    hClose appState.subprocessStderr
    timeoutStream appState "stderr" $ wait stderrHandler

    cancel cmdHandler

    exitWith exitCode

archiveName :: AppState -> SnapshotCliArgs -> Text -> Text
archiveName appState snapshotArgs hash = toText appState.jobName <> maybe "" ("-"<>) snapshotArgs.cacheVersion <> "-" <> hash <> ".tar.zst"

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

timeoutStream :: AppState -> Text -> IO () -> IO ()
timeoutStream appState streamName action = do
  result <- timeout (appState.settings.outputStreamTimeout * 1000000) action
  when (isNothing result) do
    logError appState $ "Task did not close " <> streamName <> " " <> show appState.settings.outputStreamTimeout <> " seconds after exiting."
    logError appState "Perhaps there's a background process?"

    exitFailure

outputStreamHandler :: AppState -> Handle -> ByteString -> Handle -> IO ()
outputStreamHandler appState toplevelOutput streamName stream = do
  handle ignoreEOF $ forever do
    line <- B8.hGetLine stream
    outputLine appState toplevelOutput streamName line

commandHandler :: AppState -> Handle -> Handle -> IO ()
commandHandler appState requestPipe responsePipe =
  handle ignoreEOF $ forever do
    requestLine <- B8.hGetLine requestPipe
    case Aeson.eitherDecode @[String] (BL.fromStrict requestLine) of
      Left err -> do
        logError appState $ "Invalid command pipe request: " <> show requestLine <> "\nError: " <> show err
        B8.hPutStrLn responsePipe "exit 1"
      Right cmd -> do
        logDebug appState $ "Running cmdpipe command: " <> show cmd
        result <- case cmd of
          "snapshot":sargs -> do
            case SnapshotCliArgs.parse sargs of
              Left err -> do
                logError appState $ "snapshot: " <> toText err
                pure "exit 1"
              Right args -> do
                response <- snapshot appState args
                  `catch` (\(e :: SomeException) -> do
                      logError appState $ "snapshot command failed with exception: " <> show e
                      pure "exit 1")
                pure (encodeUtf8 response)
          _ -> do
            logError appState $ "Unknown command in command pipe: " <> show cmd
            pure "exit 1"
        logDebug appState $ "cmdpipe command result: " <> show result
        B8.hPutStrLn responsePipe result

hasInputs :: SnapshotCliArgs -> Bool
hasInputs args = not (null args.fileInputs) || not (null args.rawInputs)

hasOutputs :: SnapshotCliArgs -> Bool
hasOutputs args = not (null args.outputs)

snapshot :: AppState -> SnapshotCliArgs -> IO String
snapshot appState args = do
  -- TODO: check for duplicate
  writeIORef appState.snapshotArgsRef (Just args)

  if hasInputs args then do
    logDebug appState $ "Files to hash: " <> show args.fileInputs
    logDebug appState $ "Raw inputs: " <> show args.rawInputs

    filesHashInput <-
      if not (null args.fileInputs) then
        hashFileInputs appState args.fileInputs
      else
        pure ""

    let rawHashInput = Text.intercalate "\n" (toText <$> args.rawInputs)

    let currentHashInput = filesHashInput <> rawHashInput
    let currentHash = hexSha1 currentHashInput

    savedHash <- Text.takeWhile (/= '\n') . fromMaybe "none" <$> readFileIfExists (hashFilename appState)

    if currentHash /= savedHash then do
      logDebug appState $ "Hash mismatch, saved=" <>  savedHash <> ", current=" <>  currentHash
      fromRemote <-
        if hasOutputs args then do
          s <- RemoteCache.getRemoteCacheSettingsFromEnv
          RemoteCache.restoreCache appState s (fromMaybe "." args.cacheRoot) (archiveName appState args currentHash)
        else
          pure False

      if fromRemote then do
        logDebug appState "Remote cache found, skipping"
        pure "exit 0"
      else do
        logDebug appState "Neither local nor remote cache found, running task"
        writeIORef appState.hashToSaveRef $ Just $ HashInfo currentHash currentHashInput

        when (hasOutputs args && args.fuzzyCache) do
          s <- RemoteCache.getRemoteCacheSettingsFromEnv
          let
            go [] = pure ()
            go (branch:xs) = do
              m_latestHash <- getLatestBuildHash appState s (toText appState.jobName) branch
              case m_latestHash of
                Nothing ->
                  go xs
                Just hash -> do
                  success <- RemoteCache.restoreCache appState s (fromMaybe "." args.cacheRoot) (archiveName appState args hash)
                  if success then
                    logDebug appState $ "Restored fuzzy cache from branch " <> branch <> ", hash=" <> hash
                  else
                    go xs

          getBranchesToTry appState >>= go

        pure "true"
    else do
      logDebug appState $ "Hash matches, hash=" <> savedHash <> ", skipping"
      pure "exit 0"

  else do
    pure "true"

getBranchesToTry :: AppState -> IO [Text]
getBranchesToTry appState = do
  currentBranch <- getCurrentBranch appState

  -- TODO: add fallback branches from config
  pure [currentBranch]

getCurrentBranch :: AppState -> IO Text
getCurrentBranch appState =
  bracket (hDuplicate appState.subprocessStderr) hClose \stderr_ ->
    Text.strip . Text.pack <$> readCreateProcess
      (proc "git" ["symbolic-ref", "--short", "HEAD"]) { std_err = UseHandle stderr_ }
       ""

readFileIfExists :: FilePath -> IO (Maybe Text)
readFileIfExists fp = do
  exists <- doesFileExist fp
  if exists then Just <$> Text.readFile fp else pure Nothing

hashFilename :: AppState -> String
hashFilename appState = appState.settings.stateDirectory </> "hash" </> (appState.jobName <> ".hash")

hashFileInputs :: AppState -> [FilePath] -> IO Text
hashFileInputs appState inputs =
  bracket (hDuplicate appState.subprocessStderr) hClose \stderr_ ->
    Text.strip . Text.pack <$> readCreateProcess
      (proc "bash" (["-c", $(embedStringFile "src/hash-files.sh"), "hash-files"] <> inputs))
        { std_err = UseHandle stderr_ }
       ""

ignoreEOF :: IOError -> IO ()
ignoreEOF e | isEOFError e = pure ()
            | otherwise    = throwIO e

hexSha1 :: Text -> Text
hexSha1 str = show (H.hash (encodeUtf8 str :: ByteString) :: H.Digest H.SHA1)
