import Universum

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import qualified Data.ByteString.Lazy as LBS
import System.FilePath (takeBaseName, replaceExtension)
import Data.Char (toUpper)
import qualified Data.ByteString as BS

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  inputFiles <- findByExtension [".txt"] "test/t"
  return $ testGroup "tests"
    [ goldenVsString
        (takeBaseName inputFile) -- test name
        outputFile -- golden file path
        (LBS.readFile inputFile >>= runTest) -- action whose result is tested
    | inputFile <- inputFiles
    , let outputFile = replaceExtension inputFile ".out"
    ]

runTest :: LBS.ByteString -> IO LBS.ByteString
runTest s = do
  pure ""
  -- TODO: create log tmpdir, call process, get output, get logfiles, delete log dir
