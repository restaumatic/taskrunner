module RemoteCache where

import Universum

import Control.Monad.Trans.Resource (MonadResource)
import Amazonka.Env (newEnv, Env'(..), overrideService)
import Amazonka.S3 (BucketName(..), ObjectKey(..), newGetObject, _NoSuchKey)
import Amazonka.S3.GetObject (GetObjectResponse(..))
import qualified Data.ByteString as BS
import Data.Conduit ((.|), ConduitT, bracketP, runConduitRes)
import qualified Data.Conduit.Zstd as Zstd
import Amazonka.Logger (newLogger)
import Amazonka (LogLevel(..), ResponseBody(..))
import Amazonka.Auth (fromKeys)
import Amazonka.Endpoint (setEndpoint)
import Amazonka.S3.CreateMultipartUpload (newCreateMultipartUpload)
import Amazonka.S3.StreamingUpload (streamUpload)
import Control.Exception (throwIO)
import System.Environment (lookupEnv)
import Amazonka.Types ( Region(..), AccessKey(..), SecretKey(..), Service, s3AddressingStyle, S3AddressingStyle(..) )
import Types
import System.Process (CreateProcess(..), cleanupProcess, createProcess_, StdStream (..), proc, waitForProcess)
import Conduit (sourceHandle, sinkHandle)
import Network.URI (parseURI, URI (..), URIAuth(..))
import System.Directory (makeAbsolute)
import System.FilePath (makeRelative)
import qualified System.FilePath as FP
import Utils (bail, logDebug)
import qualified Amazonka as AWS
import Control.Exception.Lens (handling)
import System.Exit (ExitCode(..))

packTar :: MonadResource m => AppState -> FilePath -> [FilePath] -> ConduitT () BS.ByteString m ()
packTar appState workdir files = do
  let cmd = "tar"
  let args = ["-c"] <> files
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
  let args = ["-x"]
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
  }

getRemoteCacheSettingsFromEnv :: IO RemoteCacheSettings
getRemoteCacheSettingsFromEnv = do
  s3Endpoint <- maybe "https://s3.amazonaws.com" toText <$> lookupEnv "TASKRUNNER_S3_ENDPOINT"
  awsRegion <- maybe "eu-central-1" toText <$> lookupEnv "TASKRUNNER_AWS_REGION"
  awsAccessKey <- maybe (error "TASKRUNNER_AWS_ACCESS_KEY not provided") toText <$> lookupEnv "TASKRUNNER_AWS_ACCESS_KEY"
  awsSecretKey <- maybe (error "TASKRUNNER_AWS_SECRET_KEY not provided") toText <$> lookupEnv "TASKRUNNER_AWS_SECRET_KEY"
  remoteCacheBucket <- maybe (error "TASKRUNNER_REMOTE_CACHE_BUCKET not provided") toText <$> lookupEnv "TASKRUNNER_REMOTE_CACHE_BUCKET"
  remoteCachePrefix <- maybe "taskrunner-cache/" toText <$> lookupEnv "TASKRUNNER_REMOTE_CACHE_PREFIX"
  pure RemoteCacheSettings{..}

parseEndpoint :: Text -> Maybe (Service -> Service)
parseEndpoint s = do
  uri <- parseURI (toString s)
  authority <- uri.uriAuthority
  ':':portStr <- pure authority.uriPort
  port <- readMaybe portStr
  pure $ setEndpoint (uri.uriScheme == "https") (encodeUtf8 authority.uriRegName) port

-- TODO:
-- - report speed, size etc.
-- - more debug logging
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
    let objectKey = settings.remoteCachePrefix <> archiveName

    cacheRoot <- makeAbsolute relativeCacheRoot

    filesRelativeToCacheRoot <- forM files \filepath -> do
      -- filepath is relative to cwd
      absFilepath <- makeAbsolute filepath
      let relative = makeRelative cacheRoot absFilepath
      when (FP.isAbsolute relative) do
        bail $ "Path " <> absFilepath <> " is outside cacheRoot (" <> cacheRoot <> ")"
      pure relative

    logDebug appState $ "Uploading to s3://" <> bucket <> "/" <> objectKey

    runConduitRes do
        let multipartUpload = newCreateMultipartUpload (BucketName bucket) (ObjectKey objectKey)
        result <-
          packTar appState cacheRoot filesRelativeToCacheRoot
          .| Zstd.compress 9
          .| streamUpload env Nothing multipartUpload
        case result of
          Left (_, err) ->
            liftIO $ throwIO err
          Right _ -> do
            liftIO $ logDebug appState "Upload success"
            pure ()

restoreCache
  :: AppState
  -> RemoteCacheSettings
  -> FilePath -- ^ Cache root (can be outside rootDirectory)
  -> Text -- Archive name
  -> IO Bool
restoreCache appState settings cacheRoot archiveName = do
  env <- newAwsEnv appState settings
  let bucket = settings.remoteCacheBucket
  let objectKey = settings.remoteCachePrefix <> archiveName

  logDebug appState $ "Downloading from s3://" <> bucket <> "/" <> objectKey

  let
    onNoSuchKey _ = do
      logDebug appState $ "Remote cache archive not found s3://" <> bucket <> "/" <> objectKey
      pure False

  handling _NoSuchKey onNoSuchKey $ runConduitRes do
    response <- AWS.send env $ newGetObject (BucketName bucket) (ObjectKey objectKey)
    response.body.body
          .| Zstd.decompress
          .| unpackTar appState cacheRoot
    pure True


newAwsEnv :: AppState -> RemoteCacheSettings -> IO AWS.Env
newAwsEnv appState settings = do
    logger <- newLogger Info appState.subprocessStderr
    let endpointFn = fromMaybe (error "invalid TASKRUNNER_S3_ENDPOINT") $ parseEndpoint settings.s3Endpoint
    newEnv (pure . fromKeys (AccessKey (encodeUtf8 settings.awsAccessKey)) (SecretKey (encodeUtf8 settings.awsSecretKey)))
        <&> (\env -> env
            { region = Region' settings.awsRegion
            , logger = logger
            })
        . overrideService (endpointFn . (\svc -> svc { s3AddressingStyle = S3AddressingStylePath }))
