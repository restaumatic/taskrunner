{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module RemoteCache where

import Universum

import Control.Monad.Trans.Resource (MonadResource)
import Amazonka.Env (newEnv, Env'(..), overrideService)
import Amazonka.S3 (BucketName(..), ObjectKey(..), newGetObject, _NoSuchKey, StorageClass (StorageClass_REDUCED_REDUNDANCY))
import Amazonka.S3.GetObject (GetObjectResponse(..))
import qualified Data.ByteString as BS
import Data.Conduit ((.|), ConduitT, bracketP, runConduitRes)
import qualified Data.Conduit.Zstd as Zstd
import Amazonka.Logger (newLogger)
import Amazonka (LogLevel(..), ResponseBody(..))
import Amazonka.Auth (fromKeys)
import Amazonka.Endpoint (setEndpoint)
import Amazonka.S3.CreateMultipartUpload (newCreateMultipartUpload, CreateMultipartUpload(..))
import Amazonka.S3.StreamingUpload (streamUpload)
import Control.Exception (throwIO)
import System.Environment (lookupEnv)
import Amazonka.Types ( Region(..), AccessKey(..), SecretKey(..), Service, s3AddressingStyle, S3AddressingStyle(..) )
import Types
import System.Process (CreateProcess(..), cleanupProcess, createProcess_, StdStream (..), proc, waitForProcess)
import Conduit (sourceHandle, sinkHandle, foldMapC)
import Network.URI (parseURI, URI (..), URIAuth(..))
import System.Directory (makeAbsolute, canonicalizePath)
import System.FilePath (makeRelative)
import qualified System.FilePath as FP
import Utils (bail, bytesfmt, formatSeconds, logDebug, logFileName, logInfo, timed, transferSummary, withStderrPipe)
import qualified Amazonka as AWS
import Control.Exception.Lens (handling)
import System.Exit (ExitCode(..))
import qualified Data.Conduit as C
import qualified Data.Conduit.Text as CT
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Amazonka.S3.PutObject (newPutObject, PutObject(..))
import GHC.Clock (getMonotonicTime)
import ParallelUnpack (unpackTarParallel)


packTar :: MonadResource m => AppState -> Handle -> FilePath -> [FilePath] -> ConduitT () BS.ByteString m ()
packTar appState stderrHandle workdir files = do
  let cmd = "tar"
  let args = if null files then ["-c", "--files-from=/dev/null"] else ["-c"] <> files
  liftIO $ logDebug appState $ "Running subprocess: " <> show (cmd:args) <> " in cwd " <> show workdir
  bracketP ( createProcess_ "createProcess_"
    (proc cmd args)
      { std_out = CreatePipe
      , std_err = UseHandle stderrHandle
      , cwd = Just workdir
      }
     ) cleanupProcess \case
       (_, Just stdoutPipe, _, process) -> do
         sourceHandle stdoutPipe
         exitCode <- liftIO $ waitForProcess process
         when (exitCode /= ExitSuccess) do
           liftIO $ bail $ "tar pack command failed with code: " <> show exitCode
       _ ->
         error "unable to obtain stdout pipe"

-- | Unpack a compressed archive into @workdir@.
--
-- With more than one unpack worker the stream is decompressed here and split
-- across concurrent tar processes, which is much faster for the many-small-files
-- trees that caches usually hold. The archive format is the same either way, so
-- this reads bundles saved by any version.
--
-- Decompressing here also means a tap can go between zstd and tar, which is what
-- lets 'restoreCache' tell decompression apart from the file writes. The
-- single-process path cannot: there zstd runs inside tar.
unpack
  :: MonadResource m
  => AppState -> Handle -> FilePath
  -> IORef TransferStats -- ^ Tap on the decompressed stream, parallel path only
  -> ConduitT BS.ByteString Void m ()
unpack appState stderrHandle workdir decompressedStatsRef
  | appState.settings.unpackWorkers > 1 =
      Zstd.decompress
        .| measureTransfer decompressedStatsRef
        .| unpackTarParallel appState stderrHandle workdir appState.settings.unpackWorkers
  | otherwise =
      unpackTar appState stderrHandle workdir

unpackTar :: MonadResource m => AppState -> Handle -> FilePath -> ConduitT BS.ByteString Void m ()
unpackTar appState stderrHandle workdir = do
  let cmd = "tar"
  let args = ["-x", "--zstd"]
  liftIO $ logDebug appState $ "Running subprocess: " <> show (cmd:args) <> " in cwd " <> show workdir
  bracketP ( createProcess_ "createProcess_"
    (proc cmd args)
      { std_in = CreatePipe
      , std_err = UseHandle stderrHandle
      , cwd = Just workdir
      }
     ) cleanupProcess \case
       (Just stdinPipe, _, _, process) -> do
         sinkHandle stdinPipe
         -- tar is still extracting after we hand over the last byte, and that
         -- tail is outside the pipeline's own accounting, so report it too.
         -- Otherwise the figures visibly fail to add up to the elapsed time.
         (_, drainSeconds) <- timed do
           hClose stdinPipe
           exitCode <- liftIO $ waitForProcess process
           when (exitCode /= ExitSuccess) do
             liftIO $ bail $ "tar unpack command failed with code: " <> show exitCode
         liftIO $ logDebug appState $ "tar drained in " <> formatSeconds drainSeconds
           <> " after the last byte"
       _ ->
         error "unable to obtain stdin pipe"

data RemoteCacheSettings = RemoteCacheSettings
  { s3Endpoint :: Text
  , awsRegion :: Text
  , awsAccessKey :: Text
  , awsSecretKey :: Text

  , remoteCacheBucket :: Text
  , remoteCachePrefix :: Text

  , logsPrefix :: Text
  , logsViewUrl :: Text
  }

getRemoteCacheSettingsFromEnv :: MonadIO m => m RemoteCacheSettings
getRemoteCacheSettingsFromEnv = liftIO do
  s3Endpoint <- maybe "https://s3.amazonaws.com" toText <$> lookupEnv "TASKRUNNER_S3_ENDPOINT"
  awsRegion <- maybe "eu-central-1" toText <$> lookupEnv "TASKRUNNER_AWS_REGION"
  awsAccessKey <- maybe (error "TASKRUNNER_AWS_ACCESS_KEY not provided") toText <$> lookupEnv "TASKRUNNER_AWS_ACCESS_KEY"
  awsSecretKey <- maybe (error "TASKRUNNER_AWS_SECRET_KEY not provided") toText <$> lookupEnv "TASKRUNNER_AWS_SECRET_KEY"
  remoteCacheBucket <- maybe (error "TASKRUNNER_REMOTE_CACHE_BUCKET not provided") toText <$> lookupEnv "TASKRUNNER_REMOTE_CACHE_BUCKET"
  remoteCachePrefix <- maybe "taskrunner/" toText <$> lookupEnv "TASKRUNNER_REMOTE_CACHE_PREFIX"
  logsPrefix <- maybe (error "TASKRUNNER_LOGS_PREFIX not provided") toText <$> lookupEnv "TASKRUNNER_LOGS_PREFIX"
  logsViewUrl <- maybe (error "TASKRUNNER_LOGS_VIEW_URL not provided") toText <$> lookupEnv "TASKRUNNER_LOGS_VIEW_URL"
  pure RemoteCacheSettings{..}

parseEndpoint :: Text -> Maybe (Service -> Service)
parseEndpoint "default-aws" = Just id
parseEndpoint s = do
  uri <- parseURI (toString s)
  authority <- uri.uriAuthority
  ':':portStr <- pure authority.uriPort
  port <- readMaybe portStr
  pure $ setEndpoint (uri.uriScheme == "https") (encodeUtf8 authority.uriRegName) port
    . (\svc -> svc { s3AddressingStyle = S3AddressingStylePath })

-- TODO:
-- - integrate amazonka logging
-- - handle errors
saveCache
  :: AppState
  -> RemoteCacheSettings
  -> FilePath -- ^ Cache root (can be outside rootDirectory)
  -> [FilePath] -- ^ Files to archive, relative to cwd (not cache root!)
  -> Text -- Archive name
  -> IO ()
saveCache appState settings relativeCacheRoot files archiveName = do
    env <- newAwsEnv appState settings

    let bucket = settings.remoteCacheBucket
    let objectKey = settings.remoteCachePrefix <> "bundles/" <> archiveName

    cacheRoot <- makeAbsolute relativeCacheRoot >>= canonicalizePath

    logDebug appState $ "Cache root: " <> show cacheRoot

    filesRelativeToCacheRoot <- forM files \filepath -> do
      -- filepath is relative to cwd
      absFilepath <- makeAbsolute filepath >>= canonicalizePath
      logDebug appState $ "makeAbsolute: " <> show filepath <> " -> " <> show absFilepath
      let relative = makeRelative cacheRoot absFilepath
      when (FP.isAbsolute relative) do
        bail $ "Path " <> absFilepath <> " is outside cacheRoot (" <> cacheRoot <> ")"
      pure relative

    logDebug appState $ "Uploading to s3://" <> bucket <> "/" <> objectKey

    -- Taps either side of zstd, so a slow save can be pinned on reading the
    -- files, on compressing them, or on the network.
    packStatsRef <- newIORef emptyTransferStats
    uploadStatsRef <- newIORef emptyTransferStats

    (_, elapsed) <- timed $ withStderrPipe appState \stderrHandle ->
      runConduitRes do
        let multipartUpload = (newCreateMultipartUpload (BucketName bucket) (ObjectKey objectKey) :: CreateMultipartUpload)
              { storageClass = Just StorageClass_REDUCED_REDUNDANCY }
        result <-
          packTar appState stderrHandle cacheRoot filesRelativeToCacheRoot
          .| measureTransfer packStatsRef
          .| Zstd.compress 3
          .| measureTransfer uploadStatsRef
          .| streamUpload env Nothing multipartUpload
        case result of
          Left (_, err) ->
            liftIO $ throwIO err
          Right _ -> do
            liftIO $ logDebug appState "Upload success"
            pure ()

    packStats <- readIORef packStatsRef
    uploadStats <- readIORef uploadStatsRef
    -- The two taps nest rather than partition: when zstd wants input it pulls
    -- through the upstream tap, so the time the upload tap spent waiting for
    -- zstd already contains the time spent waiting for tar. Subtracting leaves
    -- compression on its own, and the three then add up to the elapsed time.
    let readingSeconds = packStats.producingSeconds
        compressionSeconds = max 0 (uploadStats.producingSeconds - packStats.producingSeconds)
        uploadSeconds = uploadStats.consumingSeconds
    -- Largest figure is the bottleneck. See 'measureTransfer' for why the
    -- network one is a lower bound.
    logDebug appState $ "Packed and uploaded " <> transferSummary uploadStats.bytes elapsed
      <> ", compressed from " <> toText (bytesfmt "%.2f" packStats.bytes)
      <> " - blocked on reading files " <> formatSeconds readingSeconds
      <> ", on compression " <> formatSeconds compressionSeconds
      <> ", on upload " <> formatSeconds uploadSeconds

-- | Bytes seen, and how the wall clock divided between producing them and
-- consuming them.
data TransferStats = TransferStats
  { bytes :: !Int
  , producingSeconds :: !Double
  , consumingSeconds :: !Double
  }

emptyTransferStats :: TransferStats
emptyTransferStats = TransferStats
  { bytes = 0
  , producingSeconds = 0
  , consumingSeconds = 0
  }

-- | Pass data through unchanged, recording how long the pipeline sat blocked
-- waiting for upstream to hand over a chunk versus blocked waiting for
-- downstream to accept one.
--
-- Conduit runs the two strictly alternately - 'C.await' returns once upstream
-- has a chunk, and 'C.yield' returns once downstream wants the next one - so
-- together these account for the pipeline's whole wall clock.
--
-- These are *stall* times, not time spent doing the work, and for I/O the two
-- differ. A @write@ to a socket returns as soon as the data is copied into the
-- kernel's send buffer; the kernel then transmits it while the next chunk is
-- being compressed. So the transfer overlaps the rest of the pipeline and is
-- undercounted here - if the pipeline is CPU-bound the writes cost almost
-- nothing. Reads are the mirror image: data accumulates in the receive buffer
-- while we are busy, so an 'await' that finds it already there costs nothing.
--
-- Read these numbers as "where did the pipeline stall", which is what identifies
-- the bottleneck. Do not read them as "how long the bytes spent in transit".
measureTransfer :: MonadIO m => IORef TransferStats -> ConduitT BS.ByteString BS.ByteString m ()
measureTransfer ref = loop
  where
  loop = do
    beforeAwait <- liftIO getMonotonicTime
    m_chunk <- C.await
    afterAwait <- liftIO getMonotonicTime
    case m_chunk of
      Nothing ->
        liftIO $ modifyIORef' ref \stats -> stats
          { producingSeconds = stats.producingSeconds + (afterAwait - beforeAwait) }
      Just chunk -> do
        C.yield chunk
        afterYield <- liftIO getMonotonicTime
        liftIO $ modifyIORef' ref \stats -> stats
          { bytes = stats.bytes + BS.length chunk
          , producingSeconds = stats.producingSeconds + (afterAwait - beforeAwait)
          , consumingSeconds = stats.consumingSeconds + (afterYield - afterAwait)
          }
        loop

data LogMode = NoLog | Log deriving (Eq, Show)

restoreCache
  :: AppState
  -> RemoteCacheSettings
  -> FilePath -- ^ Cache root (can be outside rootDirectory)
  -> Text -- Archive name
  -> LogMode
  -> IO Bool
restoreCache appState settings cacheRoot archiveName logMode = do
  env <- newAwsEnv appState settings
  let bucket = settings.remoteCacheBucket
  let objectKey = settings.remoteCachePrefix <> "bundles/" <> archiveName

  logDebug appState $ "Downloading from s3://" <> bucket <> "/" <> objectKey

  let
    onNoSuchKey _ = do
      logDebug appState $ "Remote cache archive not found s3://" <> bucket <> "/" <> objectKey
      pure False

  handling _NoSuchKey onNoSuchKey $ withStderrPipe appState \stderrHandle -> do
    statsRef <- newIORef emptyTransferStats
    decompressedStatsRef <- newIORef emptyTransferStats

    (_, elapsed) <- timed $ runConduitRes do
      response <- AWS.send env $ newGetObject (BucketName bucket) (ObjectKey objectKey)
      when (logMode == Log) do
        liftIO $ logInfo appState $ "Found remote cache " <> archiveName <> ", restoring"
      response.body.body
            .| measureTransfer statsRef
            .| unpack appState stderrHandle cacheRoot decompressedStatsRef

    stats <- readIORef statsRef
    decompressedStats <- readIORef decompressedStatsRef
    -- The size is that of the compressed archive. As on the save path the taps
    -- nest rather than partition, so decompression is the difference between
    -- them; the figures then add up to the elapsed time. Note the download side
    -- excludes connection setup, which happened above in AWS.send, and see
    -- 'measureTransfer' for why it is a lower bound.
    let downloadSeconds = stats.producingSeconds
        unpackAttribution
          -- The single-process path has no tap between zstd and tar, so the two
          -- cannot be told apart there.
          | appState.settings.unpackWorkers > 1 =
              ", on decompression "
                <> formatSeconds (max 0 (decompressedStats.producingSeconds - downloadSeconds))
                <> ", on unpacking " <> formatSeconds decompressedStats.consumingSeconds
          | otherwise =
              ", on decompression and unpacking " <> formatSeconds stats.consumingSeconds
    logDebug appState $ "Downloaded and unpacked " <> transferSummary stats.bytes elapsed
      <> " - blocked on download " <> formatSeconds downloadSeconds
      <> unpackAttribution

    pure True

getLatestBuildHash
  :: AppState
  -> RemoteCacheSettings
  -> Text -- ^ Job name
  -> Text -- ^ branch
  -> IO (Maybe Text)
getLatestBuildHash appState settings jobName branch = do
  env <- newAwsEnv appState settings
  let bucket = settings.remoteCacheBucket
  let objectKey = settings.remoteCachePrefix <> "latest/" <> jobName <> "-" <> branch <> ".txt"

  logDebug appState $ "Downloading latest hash from s3://" <> bucket <> "/" <> objectKey

  let
    onNoSuchKey _ = do
      logDebug appState $ "Latest hash key not found s3://" <> bucket <> "/" <> objectKey
      pure Nothing

  handling _NoSuchKey onNoSuchKey $ runConduitRes do
    response <- AWS.send env $ newGetObject (BucketName bucket) (ObjectKey objectKey)
    response.body.body .| Just <$> readByteStringConduitAsText

readByteStringConduitAsText :: MonadThrow m => C.ConduitT BS.ByteString Void m Text
readByteStringConduitAsText = do
    -- Decode ByteString to Text, then fold the chunks into a Builder, and finally convert to strict Text
    TL.toStrict . TLB.toLazyText <$> (CT.decodeUtf8 .| foldMapC TLB.fromText)

setLatestBuildHash
  :: AppState
  -> RemoteCacheSettings
  -> Text -- ^ Job name
  -> Text -- ^ Branch
  -> Text -- ^ Hash to store
  -> IO ()
setLatestBuildHash appState settings jobName branch hash = do
  env <- newAwsEnv appState settings
  let bucket = settings.remoteCacheBucket
  let objectKey = settings.remoteCachePrefix <> "latest/" <> jobName <> "-" <> branch <> ".txt"

  logDebug appState $ "Uploading latest hash " <> hash <> " to s3://" <> bucket <> "/" <> objectKey

  runConduitRes do
    void $ AWS.send env $ (newPutObject (BucketName bucket) (ObjectKey objectKey) (AWS.toBody hash) :: PutObject)
      { storageClass = Just StorageClass_REDUCED_REDUNDANCY }

uploadLog
  :: AppState
  -> RemoteCacheSettings
  -> IO Text
uploadLog appState settings = do
  env <- newAwsEnv appState settings
  let bucket = settings.remoteCacheBucket

  let suffix = appState.buildId <> "/" <> toText appState.jobName <> ".log.txt"
  let objectKey = settings.logsPrefix <> suffix

  logDebug appState $ "Uploading logs to s3://" <> bucket <> "/" <> objectKey

  content <- BS.readFile (logFileName appState.settings appState.buildId appState.jobName)
  runConduitRes do
    void $ AWS.send env $ (newPutObject (BucketName bucket) (ObjectKey objectKey) (AWS.toBody content) :: PutObject)
      { contentType = Just "text/plain; charset=utf-8"
      , storageClass = Just StorageClass_REDUCED_REDUNDANCY
      }

  let url = settings.logsViewUrl <> suffix

  logDebug appState $ "Logs uploaded, available at " <> url

  pure url

newAwsEnv :: AppState -> RemoteCacheSettings -> IO AWS.Env
newAwsEnv _appState settings = do
    -- TODO: use subprocessStderr
    logger <- newLogger Info stderr
    let endpointFn = fromMaybe (error "invalid TASKRUNNER_S3_ENDPOINT") $ parseEndpoint settings.s3Endpoint
    newEnv (pure . fromKeys (AccessKey (encodeUtf8 settings.awsAccessKey)) (SecretKey (encodeUtf8 settings.awsSecretKey)))
        <&> (\env -> env
            { region = Region' settings.awsRegion
            , logger = logger
            })
        . overrideService endpointFn
