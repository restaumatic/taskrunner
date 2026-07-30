{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module RemoteCache where

import Universum

import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Amazonka.Env (newEnv, Env'(..), overrideService)
import Amazonka.S3 (BucketName(..), ObjectKey(..), newGetObject, _NoSuchKey, StorageClass (StorageClass_REDUCED_REDUNDANCY))
import Amazonka.S3.GetObject (GetObject(..), GetObjectResponse(..))
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
import Conduit (sourceHandle, sinkHandle, foldMapC, sinkList)
import Network.URI (parseURI, URI (..), URIAuth(..))
import System.Directory (makeAbsolute, canonicalizePath)
import System.FilePath (makeRelative)
import qualified System.FilePath as FP
import Utils (bail, bytesfmt, logDebug, logFileName, logInfo, logWarn, timed, transferSummary, withStderrPipe)
import qualified Amazonka as AWS
import Control.Exception.Lens (handling)
import System.Exit (ExitCode(..))
import qualified Data.Conduit as C
import qualified Data.Conduit.Text as CT
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Amazonka.S3.PutObject (newPutObject, PutObject(..))
import Control.Concurrent.Prefetch (cancelPrefetch, nextPrefetch, startPrefetch)


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
         hClose stdinPipe
         exitCode <- liftIO $ waitForProcess process
         when (exitCode /= ExitSuccess) do
           liftIO $ bail $ "tar unpack command failed with code: " <> show exitCode
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

  -- | How many ranged GET requests to use when downloading a cache archive.
  -- 1 (the default) means a single plain request for the whole object.
  , s3DownloadConcurrency :: Int
  -- | How many bytes a single ranged GET request asks for.
  , s3DownloadChunkSize :: Int
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
  s3DownloadConcurrency <- lookupPositiveIntEnv "TASKRUNNER_S3_DOWNLOAD_CONCURRENCY" 1
  s3DownloadChunkSizeMiB <- lookupPositiveIntEnv "TASKRUNNER_S3_DOWNLOAD_CHUNK_SIZE_MIB" 8
  let s3DownloadChunkSize = s3DownloadChunkSizeMiB * 1024 * 1024
  pure RemoteCacheSettings{..}

lookupPositiveIntEnv :: String -> Int -> IO Int
lookupPositiveIntEnv name defaultValue =
  lookupEnv name >>= \case
    Nothing ->
      pure defaultValue
    Just str ->
      case readMaybe str of
        Just value | value > 0 ->
          pure value
        _ ->
          error $ toText name <> " must be a positive integer, got: " <> show str

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

    packedBytes <- newIORef 0
    uploadedBytes <- newIORef 0

    (_, elapsed) <- timed $ withStderrPipe appState \stderrHandle ->
      runConduitRes do
        let multipartUpload = (newCreateMultipartUpload (BucketName bucket) (ObjectKey objectKey) :: CreateMultipartUpload)
              { storageClass = Just StorageClass_REDUCED_REDUNDANCY }
        result <-
          packTar appState stderrHandle cacheRoot filesRelativeToCacheRoot
          .| countBytes packedBytes
          .| Zstd.compress 3
          .| countBytes uploadedBytes
          .| streamUpload env Nothing multipartUpload
        case result of
          Left (_, err) ->
            liftIO $ throwIO err
          Right _ -> do
            liftIO $ logDebug appState "Upload success"
            pure ()

    packed <- readIORef packedBytes
    uploaded <- readIORef uploadedBytes
    -- Note the rate covers the whole pipeline (tar, zstd and the upload), not
    -- just the network part.
    logDebug appState $ "Packed and uploaded " <> transferSummary uploaded elapsed
      <> ", compressed from " <> toText (bytesfmt "%.2f" packed)

-- | Pass data through unchanged, accumulating the total number of bytes seen.
countBytes :: MonadIO m => IORef Int -> ConduitT BS.ByteString BS.ByteString m ()
countBytes ref = C.awaitForever \chunk -> do
  modifyIORef' ref (+ BS.length chunk)
  C.yield chunk

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
    downloadedBytes <- newIORef 0

    (_, elapsed) <- timed $ runResourceT do
      source <- startDownload appState settings env (BucketName bucket) (ObjectKey objectKey)

      -- Only now that the archive is known to exist: say so, and start unpacking.
      when (logMode == Log) do
        logInfo appState $ "Found remote cache " <> archiveName <> ", restoring"

      C.runConduit $
        source
          .| countBytes downloadedBytes
          .| unpackTar appState stderrHandle cacheRoot

    downloaded <- readIORef downloadedBytes
    -- The size is that of the compressed archive, and the rate covers the whole
    -- pipeline (the download, zstd and tar), not just the network part.
    logDebug appState $ "Downloaded and unpacked " <> transferSummary downloaded elapsed

    pure True

-- | Make the initial request for an S3 object, and return a source streaming its
-- contents. Uses several parallel ranged GET requests when
-- @s3DownloadConcurrency@ is above 1: a single stream tends to be limited well
-- below the available bandwidth, so fetching a few ranges at once is noticeably
-- faster for large archives.
--
-- Chunks are emitted strictly in order, so downstream sees the same byte stream
-- either way.
--
-- Note the first request deliberately happens before the returned source is
-- consumed, so that a missing object is reported (as '_NoSuchKey') before the
-- caller starts anything else. Conduit initialises sinks before pulling from the
-- source, so folding this into the pipeline would mean 'unpackTar' had already
-- spawned tar by the time we found out, which then complains about its empty
-- input on every cache miss.
startDownload
  :: AppState
  -> RemoteCacheSettings
  -> AWS.Env
  -> BucketName
  -> ObjectKey
  -> ResourceT IO (ConduitT () BS.ByteString (ResourceT IO) ())
startDownload appState settings env bucket key
  | settings.s3DownloadConcurrency <= 1 = do
      response <- AWS.send env $ newGetObject bucket key
      pure response.body.body
  | otherwise = do
      -- The first request doubles as the existence check (so that _NoSuchKey is
      -- still thrown from here) and tells us the total size via Content-Range,
      -- which is what lets us plan the remaining ranges without a separate
      -- HeadObject request. Note that HeadObject would not do: S3 answers HEAD
      -- with an empty body, so a missing object does not come back as
      -- _NoSuchKey there.
      firstResponse <- AWS.send env $ rangedGetObject bucket key (0, fromIntegral chunkSize - 1)

      -- A 206 means the range was honoured and the body is only the first
      -- chunk; anything else (a server ignoring Range, or an object smaller
      -- than one chunk served whole) means we already have everything.
      if firstResponse.httpStatus /= 206 then
        pure firstResponse.body.body
      else case parseContentRangeTotal =<< firstResponse.contentRange of
        Nothing -> do
          -- Partial response, but we cannot tell how much is left, so we cannot
          -- safely stream this body and stop. Start over in a single request.
          logWarn appState $ "Could not determine object size from Content-Range: "
            <> show firstResponse.contentRange <> ", downloading in a single request"
          response <- AWS.send env $ newGetObject bucket key
          pure response.body.body
        Just total -> do
          logDebug appState $ "Object size: " <> toText (bytesfmt "%.2f" total)
            <> ", downloading with concurrency " <> show settings.s3DownloadConcurrency
          case chunkRanges chunkSize (fromIntegral chunkSize) total of
            [] ->
              -- Object fits in a single chunk, which we already have.
              pure firstResponse.body.body
            remainingRanges ->
              -- Start prefetching the rest right away, so it overlaps with
              -- streaming the first chunk downstream.
              pure $ bracketP
                (startPrefetch settings.s3DownloadConcurrency remainingRanges
                  (fetchRange env bucket key))
                cancelPrefetch
                \prefetch -> do
                  firstResponse.body.body
                  let go = liftIO (nextPrefetch prefetch) >>= \case
                        Nothing -> pure ()
                        Just chunk -> C.yield chunk >> go
                  go
  where
  chunkSize = max 1 settings.s3DownloadChunkSize

-- | Download a single byte range of an object into memory.
fetchRange :: AWS.Env -> BucketName -> ObjectKey -> (Integer, Integer) -> IO BS.ByteString
fetchRange env bucket key range' =
  AWS.runResourceT do
    response <- AWS.send env $ rangedGetObject bucket key range'
    BS.concat <$> C.runConduit (response.body.body .| sinkList)

-- | A GET request for an inclusive byte range, as in the HTTP @Range@ header.
rangedGetObject :: BucketName -> ObjectKey -> (Integer, Integer) -> GetObject
rangedGetObject bucket key (start, end) =
  (newGetObject bucket key)
    { range = Just $ "bytes=" <> show start <> "-" <> show end }

-- | Split @[start, total)@ into consecutive inclusive ranges of at most
-- @chunkSize@ bytes each.
chunkRanges :: Int -> Integer -> Integer -> [(Integer, Integer)]
chunkRanges chunkSize start total
  | start >= total = []
  | otherwise =
      (start, min (start + size) total - 1) : chunkRanges chunkSize (start + size) total
  where
  size = fromIntegral (max 1 chunkSize)

-- | Total object size from a @Content-Range@ header value, e.g. the 52428800 in
-- @bytes 0-8388607/52428800@. 'Nothing' if the size is unknown (@*@) or the
-- header is malformed.
parseContentRangeTotal :: Text -> Maybe Integer
parseContentRangeTotal header = do
  let total = Text.drop 1 $ Text.dropWhile (/= '/') header
  readMaybe (toString total)

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
