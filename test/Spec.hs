{-# LANGUAGE LambdaCase #-}

import Universum

import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import qualified Data.ByteString.Lazy as LBS
import System.FilePath (takeBaseName, replaceExtension)
import System.Process (proc, createPipe, CreateProcess (..), StdStream (..), withCreateProcess, waitForProcess)
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..))
import System.Environment (getEnv, lookupEnv)
import System.FilePath.Glob as Glob
import System.FilePath qualified as FP
import Data.Default (Default(..))
import Data.List (isInfixOf)
import System.Random (randomIO)
import qualified Amazonka as AWS
import qualified Amazonka.Auth as AWS
import RemoteCache (parseEndpoint)
import Amazonka (runResourceT)
import Amazonka.S3 (newCreateBucket, BucketName (..), newDeleteBucket, newListObjectsV2, newDeleteObjects, newDelete)
import Amazonka.S3.Types.Delete (Delete(..))
import Amazonka.S3.ListObjectsV2 (ListObjectsV2Response(..))
import Amazonka.S3.Types.ObjectIdentifier (newObjectIdentifier)
import Amazonka.S3.Types.Object (Object(..))
import qualified FakeGithubApi

main :: IO ()
main = defaultMain =<< goldenTests

fakeGithubPort :: Int
fakeGithubPort = 12345

goldenTests :: IO TestTree
goldenTests = do
  skipSlow <- (==Just "1") <$> lookupEnv "SKIP_SLOW_TESTS"
  inputFiles0 <- sort <$> findByExtension [".txt"] "test/t"
  let inputFiles
        | skipSlow = filter (\filename -> not ("/slow/" `isInfixOf` filename)) inputFiles0
        | otherwise = inputFiles0
  pure $ Tasty.withResource (FakeGithubApi.start fakeGithubPort) FakeGithubApi.stop \fakeGithubServer ->
    testGroup "tests"
      [ goldenVsStringDiff
          (takeBaseName inputFile) -- test name
          (\ref new -> ["diff", "-u", ref, new])
          outputFile -- golden file path
          (do
            server <- fakeGithubServer
            FakeGithubApi.clearOutput server

            source <- System.IO.readFile inputFile
            runTest server source
          )
      | inputFile <- inputFiles
      , let outputFile = replaceExtension inputFile ".out"
      ]

runTest :: FakeGithubApi.Server -> String -> IO LBS.ByteString
runTest fakeGithubServer source = do
  withSystemTempDirectory "testrunner-test" \dir -> do
    let options = getOptions (toText source)

    (pipeRead, pipeWrite) <- createPipe
    path <- getEnv "PATH"

    let
      bashArgs = [ "-e", "-c", source]
      initialProc
          | options.toplevel =
              proc "taskrunner" $
                [ "-n"
                , "toplevel"
                , "bash"
                ] <> bashArgs
          | otherwise =
              proc "bash" bashArgs

    maybeWithBucket options \s3ExtraEnv -> do
      -- Generate a fake GitHub key with command: openssl genrsa -out test/fake-github-key.pem 2048
      githubKey <- System.IO.readFile "test/fake-github-key.pem"

      withCreateProcess
        initialProc { std_out = UseHandle pipeWrite, std_err = UseHandle pipeWrite
            , env = Just
            ([ ("TASKRUNNER_STATE_DIRECTORY", dir)
              , ("TASKRUNNER_DISABLE_TIMESTAMPS", "1")
              , ("TASKRUNNER_OUTPUT_STREAM_TIMEOUT", "1")
              , ("TASKRUNNER_LOG_INFO", "1")
              -- For creating Git commits
              , ("GIT_AUTHOR_NAME", "test")
              , ("GIT_AUTHOR_EMAIL", "test@example.com")
              , ("GIT_COMMITTER_NAME", "test")
              , ("GIT_COMMITTER_EMAIL", "test@example.com")

              , ("PATH", path)
              ] <>
              mwhen options.githubKeys
                [ ("GITHUB_API_URL", "http://localhost:" <> show fakeGithubPort)
                , ("GITHUB_APP_ID", "666")
                , ("GITHUB_INSTALLATION_ID", "123")
                , ("GITHUB_APP_PRIVATE_KEY", githubKey)
                , ("GITHUB_REPOSITORY_OWNER", "fakeowner")
                , ("GITHUB_REPOSITORY", "fakerepo")
                ] <>
              s3ExtraEnv)
            , cwd = Just dir
            } \_ _ _ processHandle -> do

        output <- LBS.hGetContents pipeRead
        -- FIXME: we can probably get a deadlock if the pipe is filled (since we're not reading from it yet)

        exitCode <- waitForProcess processHandle

        checkFiles <-
          forM options.checkFileGlobs \case
            "output" ->
              pure ["-- output:\n" <> output]
            "github" -> do
              out <- FakeGithubApi.getOutput fakeGithubServer
              pure ["-- github:\n" <> encodeUtf8 (foldMap (<>"\n") out)]
            glob' -> do
              files <- globDir1 (Glob.compile (toString glob')) dir
              forM files \file -> do
                content <- LBS.readFile file
                let relativeFilename = FP.makeRelative dir file
                pure $ "-- " <> encodeUtf8 relativeFilename <> ":\n" <> content

        pure $ mconcat
          [ mconcat $ mconcat checkFiles
          , case exitCode of
              ExitSuccess -> ""
              ExitFailure code -> "-- exit code: " <> show code <> "\n"
          ]

data Options = Options
  { checkFileGlobs :: [Text]
  , toplevel :: Bool
  , s3 :: Bool
  -- | Whether to provide GitHub app credentials in environment.
  -- If github status is disabled, taskrunner should work without them.
  , githubKeys :: Bool
  }

instance Default Options where
  def = Options
    { checkFileGlobs = ["output"]
    , toplevel = True
    , s3 = False
    , githubKeys = False
    }

getOptions :: Text -> Options
getOptions source = flip execState def $ go (lines source)
  where
  go (line:rest) =
    case words line of
      "#": "check":globs -> do
        modify (\s -> s { checkFileGlobs = globs })
        go rest
      ["#", "no", "toplevel"] -> do
        modify (\s -> s { toplevel = False })
        go rest
      ["#", "s3"] -> do
        modify (\s -> s { s3 = True })
        go rest
      ["#", "github", "keys"] -> do
        modify (\s -> s { githubKeys = True })
        go rest
      -- TODO: validate?
      _ ->
        -- stop iteration
        pure ()
  go [] = pure ()

maybeWithBucket :: Options -> ([(String,String)] -> IO a) -> IO a
maybeWithBucket Options{s3=False} block = block []
maybeWithBucket Options{s3=True} block = do
  endpoint <- getEnv "TASKRUNNER_TEST_S3_ENDPOINT"
  accessKey <- getEnv "TASKRUNNER_TEST_S3_ACCESS_KEY"
  secretKey <- getEnv "TASKRUNNER_TEST_S3_SECRET_KEY"
  let endpointFn = fromMaybe (error "invalid TASKRUNNER_TEST_S3_ENDPOINT") $ parseEndpoint (toText endpoint)
  env <- AWS.newEnv (pure . AWS.fromKeys (AWS.AccessKey (encodeUtf8 accessKey)) (AWS.SecretKey (encodeUtf8 secretKey)))
      <&> AWS.overrideService (endpointFn . (\svc -> svc { AWS.s3AddressingStyle = AWS.S3AddressingStylePath }))

  randomId <- randomIO @Word64
  let bucketName = "test-bucket-" <> show randomId

  let createBucket =
        runResourceT $ AWS.send env $ newCreateBucket $ BucketName $ toText bucketName
      deleteBucket = do
        runResourceT do
          let bucket = BucketName $ toText bucketName
          listResponse <- AWS.send env $ newListObjectsV2 bucket
          when (listResponse.isTruncated == Just True) do
            error "TODO: unhandled truncated ListObjects response when deleting test bucket"
          let objects = maybe [] (map (newObjectIdentifier . (.key))) listResponse.contents
          unless (null objects) do
            void $ AWS.send env $ newDeleteObjects bucket newDelete { objects }
          AWS.send env $ newDeleteBucket bucket

  bracket_ createBucket deleteBucket $
    block
      [ ("TASKRUNNER_S3_ENDPOINT", endpoint)
      , ("TASKRUNNER_AWS_ACCESS_KEY", accessKey)
      , ("TASKRUNNER_AWS_SECRET_KEY", secretKey)
      , ("TASKRUNNER_REMOTE_CACHE_BUCKET", bucketName)
      , ("TASKRUNNER_REMOTE_CACHE_PREFIX", "")
      ]

mwhen :: Monoid a => Bool -> a -> a
mwhen True x = x
mwhen False _ = mempty
