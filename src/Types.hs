module Types where

import Universum
import SnapshotCliArgs (SnapshotCliArgs)
import Data.Aeson (FromJSON, ToJSON)

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
  , mainBranch :: Maybe Text
  , force :: Bool
  } deriving (Show)

type JobName = String

data HashInfo = HashInfo
  { hash :: Text
  , hashInput :: Text
  , mainBranchCommit :: Maybe Text
  } deriving (Show, Generic, FromJSON, ToJSON)

emptyHashInfo :: HashInfo
emptyHashInfo = HashInfo
  { hash = ""
  , hashInput = ""
  , mainBranchCommit = Nothing
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
