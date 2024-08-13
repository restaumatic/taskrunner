import Universum

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import qualified Data.ByteString.Lazy as LBS
import System.FilePath (takeBaseName, replaceExtension, (</>))
import System.Process (proc, createPipe, CreateProcess (..), StdStream (..), withCreateProcess, waitForProcess)
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..))
import System.Environment (getEnv, lookupEnv)
import System.FilePath.Glob as Glob
import System.FilePath qualified as FP
import Data.Default (Default(..))
import Data.List (isInfixOf)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  skipSlow <- (==Just "1") <$> lookupEnv "SKIP_SLOW_TESTS"
  inputFiles0 <- findByExtension [".txt"] "test/t"
  let inputFiles
        | skipSlow = filter (\filename -> not ("/slow/" `isInfixOf` filename)) inputFiles0
        | otherwise = inputFiles0
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

    withCreateProcess
      initialProc { std_out = UseHandle pipeWrite, std_err = UseHandle pipeWrite
          , env = Just
            [ ("TASKRUNNER_STATE_DIRECTORY", dir)
            , ("TASKRUNNER_DISABLE_TIMESTAMPS", "1")
            , ("PATH", path)
            ]
          , cwd = Just dir
          } \_ _ _ processHandle -> do

      output <- LBS.hGetContents pipeRead
      -- FIXME: we can probably get a deadlock if the pipe is filled (since we're not reading from it yet)

      exitCode <- waitForProcess processHandle

      checkFiles <-
        forM options.checkFileGlobs \glob' ->
          if glob' == "output" then
            pure ["-- output:\n" <> output]
          else do
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
  }

instance Default Options where
  def = Options
    { checkFileGlobs = ["output"]
    , toplevel = True
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
      _ ->
        -- stop iteration
        pure ()
  go [] = pure ()
