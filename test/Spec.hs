import Universum

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import qualified Data.ByteString.Lazy as LBS
import System.FilePath (takeBaseName, replaceExtension, (</>))
import System.Process (proc, createPipe, CreateProcess (..), StdStream (..), withCreateProcess, waitForProcess)
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..))
import System.Environment (getEnv)
import System.FilePath.Glob as Glob
import System.FilePath qualified as FP

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  inputFiles <- findByExtension [".txt"] "test/t"
  return $ testGroup "tests"
    [ goldenVsString
        (takeBaseName inputFile) -- test name
        outputFile -- golden file path
        (System.IO.readFile inputFile >>= runTest) -- action whose result is tested
    | inputFile <- inputFiles
    , let outputFile = replaceExtension inputFile ".out"
    ]

runTest :: String -> IO LBS.ByteString
runTest source = do
  withSystemTempDirectory "testrunner-test" \dir -> do
    let logDir = dir </> "logs"

    (pipeRead, pipeWrite) <- createPipe
    path <- getEnv "PATH"
    withCreateProcess
      (proc "taskrunner"
        [ "-n"
        , "toplevel"
        , "bash"
        , "-e"
        , "-c"
        , source
        ]) { std_out = UseHandle pipeWrite, std_err = UseHandle pipeWrite
          , env = Just
            [ ("TASKRUNNER_LOG_DIRECTORY", logDir)
            , ("TASKRUNNER_LOCK_DIRECTORY", dir </> "locks")
            , ("TASKRUNNER_DISABLE_TIMESTAMPS", "1")
            , ("PATH", path)
            ]
          , cwd = Just dir
          } \_ _ _ processHandle -> do

      output <- LBS.hGetContents pipeRead
      -- FIXME: we can probably get a deadlock if the pipe is filled (since we're not reading from it yet)

      exitCode <- waitForProcess processHandle

      let checkFileGlobs = fromMaybe ["output"] $ getCheckFileGlobs (toText source)
      checkFiles <- 
        forM checkFileGlobs \glob' ->
          if glob' == "output" then
            pure ["-- output:\n" <> output]
          else do
            files <- globDir1 (Glob.compile glob') dir
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

getCheckFileGlobs :: Text -> Maybe [String]
getCheckFileGlobs source =
  case lines source of
    firstLine:_
      | "#": "check":globs <- words firstLine ->
        Just $ toString <$> globs
    _ -> Nothing
