module SnapshotCliArgs where

import Universum
import Data.Default (Default(..))

data SnapshotCliArgs = SnapshotCliArgs
    { longRunning :: Bool
    , unskippable :: Bool
    , fileInputs :: [FilePath]
    , rawInputs :: [String]
    , outputs :: [FilePath]
    , postUnpackCommands :: [String]
    , fuzzyCache :: Bool
    , cacheRoot :: Maybe FilePath
    , cacheVersion :: Maybe String
    , commitStatus :: Bool
    } deriving (Show)

instance Default SnapshotCliArgs where
  def = SnapshotCliArgs
    { longRunning = False
    , unskippable = False
    , fileInputs = []
    , rawInputs = []
    , outputs = []
    , postUnpackCommands = []
    , fuzzyCache = False
    , cacheRoot = Nothing
    , cacheVersion = Nothing
    , commitStatus = False
    }

type ParseError = String

-- Note: not using `optparse-applicative` because of the weird syntax where positional args behave differently before and after `--outputs`,
-- which I believe is not supported in that model
parse :: [String] -> Either ParseError SnapshotCliArgs
parse input = map fst $ flip execStateT (def :: SnapshotCliArgs, BeforeOutputs) $ go input
  where

  go (opt:xs) | opt `elem` ["-o", "--outputs"] = do
    modify (\(args, _) -> (args, AfterOutputs))
    go xs

  go (opt:arg:xs) | opt `elem` ["-c", "--cmd"] = do
    (_, phase) <- get
    case phase of
      BeforeOutputs ->
        lift $ Left "--cmd is supported only after --outputs"
      AfterOutputs ->
        modifyArgs (\s -> s { postUnpackCommands = s.postUnpackCommands <> [arg] })
    go xs

  go (opt:xs) | opt `elem` ["-l", "--long-running"] = do
    modifyArgs (\s -> s { longRunning = True })
    go xs

  -- `--unskippable` is an option, but I believe it doesn't do anything
  -- TODO: get rid of it
  go (opt:xs) | opt `elem` ["-n", "--unskippable"] = go xs

  go ("--raw":arg:xs) = do
    modifyArgs (\s -> s { rawInputs = s.rawInputs <> [arg] })
    go xs

  go ("--fuzzy-cache":xs) = do
    modifyArgs (\s -> s { fuzzyCache = True })
    go xs

  go ("--cache-root":arg:xs) = do
    modifyArgs (\s -> s { cacheRoot = Just arg })
    go xs

  go ("--cache-version":arg:xs) = do
    modifyArgs (\s -> s { cacheVersion = Just arg })
    go xs

  go ("--commit-status":xs) = do
    modifyArgs (\s -> s { commitStatus = True })
    go xs

  go (opt:_) | '-':_ <- opt =
    lift $ Left $ "Invalid option or missing argument: " <> opt

  go (path:xs) = do
    (_, phase) <- get
    case phase of
      BeforeOutputs ->
        modifyArgs (\s -> s { fileInputs = s.fileInputs <> [path] })
      AfterOutputs ->
        modifyArgs (\s -> s { outputs = s.outputs <> [path] })
    go xs

  go [] = pure ()

  modifyArgs f = modify (first f)

data ParsePhase = BeforeOutputs | AfterOutputs
