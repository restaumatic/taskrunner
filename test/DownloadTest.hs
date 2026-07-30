-- | Tests for 'downloadObject', which splits a download into parallel ranged
-- GET requests. These run against 'FakeS3' and need no S3 credentials.
module DownloadTest (tests) where

import Universum

import App (getSettings)
import Conduit (runResourceT, sinkList)
import qualified Amazonka as AWS
import Amazonka.Auth (fromKeys)
import Amazonka.Env (newEnv, Env'(..), overrideService)
import Amazonka.S3 (BucketName(..), ObjectKey(..))
import Amazonka.Types (AccessKey(..), Region(..), SecretKey(..))
import qualified Data.ByteString as BS
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Text as Text
import RemoteCache (RemoteCacheSettings(..), downloadObject, parseEndpoint)
import System.IO (IOMode(..))
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsStringDiff)
import Types

import FakeS3 (Behaviour(..), RequestLog, withFakeS3)

mib :: Int
mib = 1024 * 1024

tests :: TestTree
tests =
  goldenVsStringDiff
    "download-object"
    (\ref new -> ["diff", "-u", ref, new])
    "test/download-object.out"
    (encodeUtf8 . unlines <$> mapM runCase cases)

data Case = Case
  { name :: Text
  , behaviour :: Behaviour
  , objectSize :: Int
  , concurrency :: Int
  , chunkSize :: Int
  , expected :: Int -> Either Text ByteString -> [Maybe ByteString] -> Text
    -- ^ Given the object size, the download result and the requests the server
    -- saw, describe the outcome.
  }

cases :: [Case]
cases =
  [ Case
      { name = "several chunks"
      , behaviour = HonourRange, objectSize = 3000083, concurrency = 4, chunkSize = mib
      , expected = expectObject
      }
  , Case
      { name = "exact multiple of chunk size"
      -- Must not ask for an extra, empty range past the end.
      , behaviour = HonourRange, objectSize = 2 * mib, concurrency = 4, chunkSize = mib
      , expected = expectObject
      }
  , Case
      { name = "one byte over a chunk boundary"
      , behaviour = HonourRange, objectSize = mib + 1, concurrency = 4, chunkSize = mib
      , expected = expectObject
      }
  , Case
      { name = "more chunks than concurrency"
      -- The prefetch window has to slide, rather than deadlock or reorder.
      , behaviour = HonourRange, objectSize = 10 * mib, concurrency = 2, chunkSize = mib
      , expected = expectObject
      }
  , Case
      { name = "smaller than one chunk"
      , behaviour = HonourRange, objectSize = 100, concurrency = 4, chunkSize = mib
      , expected = expectObject
      }
  , Case
      { name = "concurrency 1 does not use ranges"
      , behaviour = HonourRange, objectSize = 3000083, concurrency = 1, chunkSize = mib
      , expected = \size result requests ->
          expectObject size result requests <> ", range headers: " <> show requests
      }
  , Case
      { name = "server ignores Range"
      -- We must notice we got the whole object, and not truncate it.
      , behaviour = IgnoreRange, objectSize = 3000083, concurrency = 4, chunkSize = mib
      , expected = expectObject
      }
  , Case
      { name = "server hides the object size"
      -- Nothing to base the ranges on, so it should start over unranged.
      , behaviour = UnknownTotal, objectSize = 3000083, concurrency = 4, chunkSize = mib
      , expected = expectObject
      }
  , Case
      { name = "a chunk fails"
      -- Must fail loudly rather than hand a truncated archive to tar. The
      -- request count is not checked, since amazonka retries and the remaining
      -- chunks may or may not have been started.
      , behaviour = FailAtOffset mib, objectSize = 3000083, concurrency = 4, chunkSize = mib
      , expected = \_ result _ -> case result of
          Left _ -> "failed, as expected"
          Right bytes -> "SUCCEEDED UNEXPECTEDLY with " <> show (BS.length bytes) <> " bytes"
      }
  ]

-- | Check we got the object back byte for byte, and report how many requests it
-- took (which is the point of the whole exercise).
expectObject :: Int -> Either Text ByteString -> [Maybe ByteString] -> Text
expectObject size result requests =
  case result of
    Left err ->
      "FAILED: " <> err
    Right bytes
      | bytes == payload size ->
          "ok in " <> show (length requests) <> " request(s)"
      | BS.length bytes /= size ->
          "WRONG LENGTH: got " <> show (BS.length bytes) <> ", expected " <> show size
      | otherwise ->
          "WRONG CONTENT (right length) - chunks reordered or overlapping?"

runCase :: Case -> IO Text
runCase testCase = do
  let object = payload testCase.objectSize
  appState <- mkAppState
  withFakeS3 testCase.behaviour object \requestLog port -> do
    env <- mkEnv port
    result <- tryDownload appState (mkSettings port testCase) env
    requests <- readIORef requestLog
    pure $ testCase.name <> ": " <> testCase.expected testCase.objectSize result requests

tryDownload
  :: AppState -> RemoteCacheSettings -> AWS.Env -> IO (Either Text ByteString)
tryDownload appState settings env = do
  result <- try @IO @SomeException $ runResourceT $ C.runConduit $
    downloadObject appState settings env (BucketName "bucket") (ObjectKey "obj") pass
      .| (BS.concat <$> sinkList)
  pure $ first (Text.unwords . Text.words . Text.take 200 . show) result

-- | Deterministic filler that zstd cannot squash, so that test objects actually
-- stay big enough to span several chunks.
payload :: Int -> ByteString
payload size = BS.pack $ take size $ cycle
  [fromIntegral (i * 7 + i `div` 251) | i <- [0 :: Int .. 4095]]

mkEnv :: Int -> IO AWS.Env
mkEnv port = do
  let endpoint = "http://localhost:" <> show port
      endpointFn = fromMaybe (error "invalid endpoint") $ parseEndpoint endpoint
  newEnv (pure . fromKeys (AccessKey "key") (SecretKey "secret"))
    -- Silent logger: one case deliberately provokes a server error, and it
    -- should not look like the test itself went wrong.
    <&> (\env -> env { region = Region' "eu-central-1", logger = \_ _ -> pass })
      . overrideService endpointFn

mkSettings :: Int -> Case -> RemoteCacheSettings
mkSettings port testCase = RemoteCacheSettings
  { s3Endpoint = "http://localhost:" <> show port
  , awsRegion = "eu-central-1"
  , awsAccessKey = "key"
  , awsSecretKey = "secret"
  , remoteCacheBucket = "bucket"
  , remoteCachePrefix = ""
  , logsPrefix = ""
  , logsViewUrl = ""
  , s3DownloadConcurrency = testCase.concurrency
  , s3DownloadChunkSize = testCase.chunkSize
  }

-- | Just enough 'AppState' for the logging that 'downloadObject' does. Log
-- output is discarded, so that it cannot end up in the golden output.
mkAppState :: IO AppState
mkAppState = do
  settings <- getSettings
  hashToSaveRef <- newIORef Nothing
  snapshotArgsRef <- newIORef Nothing
  skipped <- newIORef False
  quietBuffer <- newIORef []
  githubClient <- newIORef Nothing
  devNull <- openFile "/dev/null" WriteMode
  pure AppState
    { settings
    , jobName = "download-test"
    , buildId = "test"
    , isToplevel = True
    , hashToSaveRef
    , snapshotArgsRef
    , skipped
    , toplevelStderr = devNull
    , logOutput = devNull
    , quietBuffer
    , githubClient
    }
