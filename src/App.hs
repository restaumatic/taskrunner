{-# LANGUAGE TemplateHaskell #-}

module App where

import Universum

import System.Environment (setEnv, lookupEnv, getEnvironment)
import System.Process (createProcess_, CreateProcess (..), StdStream (CreatePipe, UseHandle), proc, waitForProcess, createPipe,  readCreateProcess, withCreateProcess)
import System.IO
    ( openBinaryFile, hSetBuffering, BufferMode(..), hFlush )
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import System.Directory ( createDirectoryIfMissing, doesFileExist, getCurrentDirectory, createDirectory )
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
import CommitStatus (updateCommitStatus, StatusRequest (..))
import qualified System.Process as Process
import Control.Monad.EarlyReturn (withEarlyReturn, earlyReturn)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.Clock (getCurrentTime)
import qualified Data.ByteString.Lazy.Char8 as BL8

getSettings :: IO Settings
getSettings = do
  stateDirectory <- fromMaybe "/tmp/taskrunner" <$> lookupEnv "TASKRUNNER_STATE_DIRECTORY"
  cwd <- getCurrentDirectory
  rootDirectory <- fromMaybe cwd <$> lookupEnv "TASKRUNNER_ROOT_DIRECTORY"
  timestamps <- (/=Just "1") <$> lookupEnv "TASKRUNNER_DISABLE_TIMESTAMPS"
  logDebug_ <- (==Just "1") <$> lookupEnv "TASKRUNNER_DEBUG"
  logInfo_ <- (==Just "1") <$> lookupEnv "TASKRUNNER_LOG_INFO"
  outputStreamTimeout <- maybe 5 read <$> lookupEnv "TASKRUNNER_OUTPUT_STREAM_TIMEOUT"
  saveRemoteCache <- (==Just "1") <$> lookupEnv "TASKRUNNER_SAVE_REMOTE_CACHE"
  enableCommitStatus <- (==Just "1") <$> lookupEnv "TASKRUNNER_ENABLE_COMMIT_STATUS"
  uploadLogs <- (==Just "1") <$> lookupEnv "TASKRUNNER_UPLOAD_LOGS"
  fuzzyCacheFallbackBranches <- maybe [] (Text.words . toText) <$> lookupEnv "TASKRUNNER_FALLBACK_BRANCHES"
  primeCacheMode <- (==Just "1") <$> lookupEnv "TASKRUNNER_PRIME_CACHE_MODE"
  pure Settings
        { stateDirectory
        , rootDirectory
        , timestamps
        , logDebug = logDebug_
        , logInfo = logInfo_
        , outputStreamTimeout
        , saveRemoteCache
        , enableCommitStatus
        , uploadLogs
        , fuzzyCacheFallbackBranches
        , primeCacheMode
        }

main :: IO ()
main = do
  args <- getCliArgs
  settings <- getSettings

  let jobName = fromMaybe (FilePath.takeFileName args.cmd) args.name

  (buildId, isToplevel) <- getBuildIdAndToplevel

  let lockFileName = settings.stateDirectory </> "locks" </> (jobName <> ".lock")

  createDirectoryIfMissing True (settings.stateDirectory </> "locks")
  createDirectoryIfMissing True (settings.stateDirectory </> "hash")
  createDirectoryIfMissing True (settings.stateDirectory </> "builds")

  let buildDir = settings.stateDirectory </> "builds" </> toString buildId

  when isToplevel do
    createDirectory buildDir
    createDirectory (buildDir </> "logs")
    createDirectory (buildDir </> "results")

  m_parentRequestPipe <- getParentRequestPipe

  logDebugParent m_parentRequestPipe $ "Starting subtask " <> toText jobName

  withFileLock lockFileName Exclusive \_ -> do
    whenJustM (readResultFile buildDir jobName) \exitCode -> do
      logDebugParent m_parentRequestPipe $ "Subtask " <> toText jobName <> " finished with " <> show exitCode
      exitWith exitCode

    -- Lock (take) it while writing a line to either `logFile` or stdout
    logFile <- openBinaryFile (logFileName settings buildId jobName) WriteMode
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

    appState <- AppState settings jobName buildId isToplevel <$> newIORef Nothing <*> newIORef Nothing <*> newIORef False <*> pure toplevelStderr <*> pure subprocessStderr <*> pure logFile

    cmdHandler <- async $ commandHandler appState requestPipeRead responsePipeWrite

    cwd <- getCurrentDirectory

    logDebug appState $ "Running command: " <> show (args.cmd : args.args)
    logDebug appState $ "  buildId: " <> show buildId
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

    skipped <- readIORef appState.skipped

    logDebug appState $ "Command " <> show (args.cmd : args.args) <> " exited with code " <> show exitCode
    logDebugParent m_parentRequestPipe $ "Subtask " <> toText jobName <> " finished with " <> show exitCode

    m_hashToSave <- readIORef appState.hashToSaveRef

    -- Only be chatty about exit status if we were chatty about starting the work, i.e. if it is cacheable.
    when (not skipped && isJust m_hashToSave) do
      if exitCode == ExitSuccess then
        logInfo appState "success"
      else
        logError appState $ "Failed, exit code: " <> show (exitCodeToInt exitCode)

    writeFile (buildDir </> "results" </> jobName) (show (exitCodeToInt exitCode))

    let isSuccess = exitCode == ExitSuccess
    m_snapshotArgs <- readIORef appState.snapshotArgsRef

    when isSuccess do
      whenJustM (readIORef appState.hashToSaveRef) \h -> do
        logDebug appState $ "Saving hash " <> h.hash <> " to " <> toText (hashFilename appState)
        Text.writeFile (hashFilename appState) (h.hash <> "\n\n" <> h.hashInput)

        whenJust m_snapshotArgs \snapshotArgs -> do
          forM_ snapshotArgs.postUnpackCommands \cmd -> do
            runPostUnpackCmd appState cmd

          when (hasOutputs snapshotArgs && settings.saveRemoteCache && (not skipped || appState.settings.primeCacheMode)) do
            logDebug appState "Saving remote cache"
            s <- RemoteCache.getRemoteCacheSettingsFromEnv
            RemoteCache.saveCache appState s (fromMaybe settings.rootDirectory snapshotArgs.cacheRoot) snapshotArgs.outputs (archiveName appState snapshotArgs h.hash)

            when snapshotArgs.fuzzyCache do
              branch <- getCurrentBranch appState
              RemoteCache.setLatestBuildHash appState s (toText appState.jobName) branch h.hash

    timeoutStream appState "stdout" $ wait stdoutHandler

    -- We used `createProcess_`, so we must close it manually
    hClose appState.subprocessStderr
    timeoutStream appState "stderr" $ wait stderrHandler

    cancel cmdHandler

    let shouldReportStatus = (maybe False (.commitStatus) m_snapshotArgs && not skipped) || not isSuccess

    hClose logFile

    logViewUrl <-
      if settings.uploadLogs then do
        s <- RemoteCache.getRemoteCacheSettingsFromEnv
        Just <$> RemoteCache.uploadLog appState s
      else
        pure Nothing

    when (appState.settings.enableCommitStatus && shouldReportStatus) do
      updateCommitStatus appState StatusRequest
        { state = if isSuccess then "success" else "failure"
        , target_url = logViewUrl
        , description = Nothing
        , context = toText appState.jobName
        }

    exitWith exitCode

getParentRequestPipe :: IO (Maybe Handle)
getParentRequestPipe = do
  let envName = "_taskrunner_request_pipe"
  envValue <- lookupEnv envName
  case envValue of
    Nothing ->
      pure Nothing
    Just value ->
      case readMaybe value of
        Nothing ->
          bail $ "Invalid file descriptor " <> show value <> " in " <> envName
        Just fd -> do
          h <- fdToHandle fd
          -- Note: LineBuffering seems to output the newline as a separate write (!)
          -- which breaks stuff because we can get two commands from parallel jobs on the same line
          -- TODO: instead of relying on atomic writes, lock the pipe or something during a request
          hSetBuffering h (BlockBuffering Nothing)
          pure (Just h)

-- | Issue a debug message to the parent task's log (if there is a parent).
logDebugParent :: Maybe Handle -> Text -> IO ()
logDebugParent Nothing _ = pure ()
logDebugParent (Just h) msg = do
  BL8.hPutStrLn h $ Aeson.encode ["debug", msg]
  hFlush h

readResultFile :: FilePath -> JobName -> IO (Maybe ExitCode)
readResultFile buildDir jobName = do
  m_contents <- readFileIfExists (buildDir </> "results" </> jobName)
  case m_contents of
    Nothing ->
      pure Nothing
    Just s ->
      pure $ intToExitCode <$> (readMaybe s :: Maybe Int)

intToExitCode :: Int -> ExitCode
intToExitCode 0 = ExitSuccess
intToExitCode n = ExitFailure n

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n

getBuildIdAndToplevel :: IO (Text, Bool)
getBuildIdAndToplevel = do
  let envName = "_taskrunner_build_id"
  envValue <- lookupEnv envName
  case envValue of
    Nothing -> do
      -- We are the top level.
      buildId <- newBuildId
      setEnv envName (toString buildId)
      pure (buildId, True)
    Just value ->
      pure (toText value, False)

newBuildId :: IO Text
newBuildId = toText . iso8601Show <$> getCurrentTime

runPostUnpackCmd :: AppState -> String -> IO ()
runPostUnpackCmd appState cmd = do
  logDebug appState $ "Running post-unpack cmd " <> show cmd
  bracket (hDuplicate appState.subprocessStderr) hClose \stderr_ ->
    withCreateProcess (
      (Process.shell cmd)
        { std_out = UseHandle stderr_ -- TODO: really, we should have subprocesStdout also
        , std_err = UseHandle stderr_
        }
       ) \_ _ _ process -> do
           exitCode <- liftIO $ waitForProcess process
           when (exitCode /= ExitSuccess) do
             bail $ "post-unpack cmd failed with code: " <> show exitCode

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
        m_result <- case cmd of

          "snapshot":sargs -> do
            case SnapshotCliArgs.parse sargs of
              Left err -> do
                logError appState $ "snapshot: " <> toText err
                pure (Just "exit 1")
              Right args -> do
                response <- snapshot appState args
                  `catch` (\(e :: SomeException) -> do
                      logError appState $ "snapshot command failed with exception: " <> show e
                      pure "exit 1")
                pure $ Just $ encodeUtf8 response

          "debug":args -> do
            -- debug - debug message which should land in our log, but originates from a subtask

            logDebug appState (unwords (toText <$> args))
            -- No reply, because this command can be issued concurrently
            pure Nothing

          _ -> do
            logError appState $ "Unknown command in command pipe: " <> show cmd
            pure (Just "exit 1")
        whenJust m_result \result -> do
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

  (runTask, statusDescription) <- withEarlyReturn do
    unless (hasInputs args) $ earlyReturn (True, Just "not cached")

    logDebug appState $ "Files to hash: " <> show args.fileInputs
    logDebug appState $ "Raw inputs: " <> show args.rawInputs

    filesHashInput <-
      if not (null args.fileInputs) then
        liftIO $ hashFileInputs appState args.fileInputs
      else
        pure ""

    let rawHashInput = Text.intercalate "\n" (toText <$> args.rawInputs)

    let currentHashInput = filesHashInput <> rawHashInput
    let currentHash = hexSha1 currentHashInput

    savedHash <- Text.takeWhile (/= '\n') . fromMaybe "none" <$> readFileIfExists (hashFilename appState)

    when (currentHash == savedHash) do
      logDebug appState $ "Hash matches, hash=" <> savedHash <> ", skipping"
      earlyReturn (False, Just "local cache hit (?)")

    logDebug appState $ "Hash mismatch, saved=" <>  savedHash <> ", current=" <>  currentHash

    when appState.settings.primeCacheMode do
      logDebug appState "Prime cache mode, assuming task is done and skippping!"
      writeIORef appState.hashToSaveRef $ Just $ HashInfo currentHash currentHashInput
      earlyReturn (False, Nothing)

    when (hasOutputs args) do
      s <- RemoteCache.getRemoteCacheSettingsFromEnv
      success <- liftIO $ RemoteCache.restoreCache appState s (fromMaybe appState.settings.rootDirectory args.cacheRoot) (archiveName appState args currentHash) RemoteCache.Log
      when success do
        writeIORef appState.hashToSaveRef $ Just $ HashInfo currentHash currentHashInput
        logInfo appState "Restored from remote cache"
        earlyReturn (False, Just "cache hit")

    writeIORef appState.hashToSaveRef $ Just $ HashInfo currentHash currentHashInput

    logInfo appState "Inputs changed, running task"

    when (hasOutputs args && args.fuzzyCache) do
      tryRestoreFuzzyCache appState args

    pure (True, Nothing)

  when (args.commitStatus && appState.settings.enableCommitStatus) do
    updateCommitStatus appState StatusRequest
      { state = if runTask then "pending" else "success"
      , target_url = Nothing
      , description = statusDescription
      , context = toText appState.jobName
      }

  writeIORef appState.skipped (not runTask)

  pure $ if runTask then "true" else "exit 0"

tryRestoreFuzzyCache :: MonadIO m => AppState -> SnapshotCliArgs -> m ()
tryRestoreFuzzyCache appState args = do
  s <- RemoteCache.getRemoteCacheSettingsFromEnv
  let
    go [] = pure ()
    go (branch:xs) = do
      m_latestHash <- getLatestBuildHash appState s (toText appState.jobName) branch
      case m_latestHash of
        Nothing ->
          go xs
        Just hash -> do
          success <- RemoteCache.restoreCache appState s
              (fromMaybe appState.settings.rootDirectory args.cacheRoot)
              (archiveName appState args hash) RemoteCache.NoLog
          if success then
            logInfo appState $ "Restored fuzzy cache from branch " <> branch <> ", hash=" <> hash
          else
            go xs

  liftIO $ getBranchesToTry appState >>= go

getBranchesToTry :: MonadIO m => AppState -> m [Text]
getBranchesToTry appState = liftIO do
  currentBranch <- getCurrentBranch appState
  pure $ currentBranch : appState.settings.fuzzyCacheFallbackBranches

readFileIfExists :: MonadIO m => FilePath -> m (Maybe Text)
readFileIfExists fp = liftIO do
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
