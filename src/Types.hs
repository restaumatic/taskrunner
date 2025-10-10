module Types where

import Universum
import SnapshotCliArgs (SnapshotCliArgs)
import Data.Aeson (FromJSON, ToJSON)
import qualified Network.HTTP.Client as HTTP
import Data.Time.Clock (UTCTime)

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
  , quietMode :: Bool
  , githubTokenRefreshThresholdSeconds :: Int
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
  , quietBuffer :: IORef [ByteString]

  -- | Lazily initialized Github client
  , githubClient :: IORef (Maybe GithubClient)
  }

-- Unfortunately the type has to live there due to circular dependencies (AppState -> GithubClient -> AppState)
data GithubClient = GithubClient
  { apiUrl :: Text
  , appId :: Text
  , installationId :: Text
  , privateKey :: Text
  , owner :: Text
  , repo :: Text
  , manager :: HTTP.Manager
  , accessToken :: Text
  , expiresAt :: UTCTime
  }
