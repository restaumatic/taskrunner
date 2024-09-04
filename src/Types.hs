module Types where

import Universum
import SnapshotCliArgs (SnapshotCliArgs)

data Settings = Settings
  { stateDirectory :: FilePath
  , rootDirectory :: FilePath
  , timestamps :: Bool
  , logDebug :: Bool
  , logInfo :: Bool
  , outputStreamTimeout :: Int
  , saveRemoteCache :: Bool
  , enableCommitStatus :: Bool
  , uploadLogs :: Bool
  , fuzzyCacheFallbackBranches :: [Text]
  , primeCacheMode :: Bool
  } deriving (Show)

type JobName = String

data HashInfo = HashInfo
  { hash :: Text
  , hashInput :: Text
  }

type BuildId = Text

data AppState = AppState
  { settings :: Settings
  , jobName :: JobName
  , buildId :: BuildId
  , isToplevel :: Bool
  , hashToSaveRef :: IORef (Maybe HashInfo)
  , snapshotArgsRef :: IORef (Maybe SnapshotCliArgs)
  , skipped :: IORef Bool
  , toplevelStderr :: Handle
  , subprocessStderr :: Handle
  , logOutput :: Handle
  }
