module Types where

import Universum
import SnapshotCliArgs (SnapshotCliArgs)

data Settings = Settings
  { stateDirectory :: FilePath
  , rootDirectory :: FilePath
  , timestamps :: Bool
  , debug :: Bool
  , outputStreamTimeout :: Int
  , saveRemoteCache :: Bool
  , enableCommitStatus :: Bool
  , uploadLogs :: Bool
  , fuzzyCacheFallbackBranches :: [Text]
  } deriving (Show)

type JobName = String

data HashInfo = HashInfo
  { hash :: Text
  , hashInput :: Text
  }

data AppState = AppState
  { settings :: Settings
  , jobName :: JobName
  , hashToSaveRef :: IORef (Maybe HashInfo)
  , snapshotArgsRef :: IORef (Maybe SnapshotCliArgs)
  , skipped :: IORef Bool
  , toplevelStderr :: Handle
  , subprocessStderr :: Handle
  , logOutput :: Handle
  }
