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
import Utils (bail, logDebug, logFileName, logInfo)
import qualified Amazonka as AWS
import Control.Exception.Lens (handling)
import System.Exit (ExitCode(..))
import qualified Data.Conduit as C
import qualified Data.Conduit.Text as CT
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Amazonka.S3.PutObject (newPutObject, PutObject(..))


packTar :: MonadResource m => AppState -> FilePath -> [FilePath] -> ConduitT () BS.ByteString m ()
packTar appState workdir files = do
  let cmd = "tar"
  let args = if null files then ["-c", "--files-from=/dev/null"] else ["-c"] <> files
  liftIO $ logDebug appState $ "Running subprocess: " <> show (cmd:args) <> " in cwd " <> show workdir
  bracketP ( createProcess_ "createProcess_"
    (proc cmd args)
      { std_out = CreatePipe
      , std_err = UseHandle appState.subprocessStderr
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

unpackTar :: MonadResource m => AppState -> FilePath -> ConduitT BS.ByteString Void m ()
unpackTar appState workdir = do
  let cmd = "tar"
  let args = ["-x", "--zstd"]
  liftIO $ logDebug appState $ "Running subprocess: " <> show (cmd:args) <> " in cwd " <> show workdir
  bracketP ( createProcess_ "createProcess_"
    (proc cmd args)
      { std_in = CreatePipe
      , std_err = UseHandle appState.subprocessStderr
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
-- - report speed, size etc.
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

    runConduitRes do
        let multipartUpload = (newCreateMultipartUpload (BucketName bucket) (ObjectKey objectKey) :: CreateMultipartUpload)
              { storageClass = Just StorageClass_REDUCED_REDUNDANCY }
        result <-
          packTar appState cacheRoot filesRelativeToCacheRoot
          .| Zstd.compress 3
          .| streamUpload env Nothing multipartUpload
        case result of
          Left (_, err) ->
            liftIO $ throwIO err
          Right _ -> do
            liftIO $ logDebug appState "Upload success"
            pure ()

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

  handling _NoSuchKey onNoSuchKey $ runConduitRes do
    response <- AWS.send env $ newGetObject (BucketName bucket) (ObjectKey objectKey)
    when (logMode == Log) do
      liftIO $ logInfo appState $ "Found remote cache " <> archiveName <> ", restoring"
    response.body.body
          .| unpackTar appState cacheRoot
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
