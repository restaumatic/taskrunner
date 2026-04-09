module Trace
  ( checkFsatrace
  , wrapWithFsatrace
  , parseTraceOutput
  , TraceEntry(..)
  , TraceOp(..)
  , filterTraceEntries
  , formatTraceReport
  ) where

import Universum

import qualified Data.Text as Text
import System.Directory (findExecutable)
import System.FilePath (makeRelative, isRelative)
import Data.List (nub)
import Utils (bail)

data TraceOp = TraceRead | TraceWrite | TraceMove | TraceDelete | TraceTouch | TraceQuery
  deriving (Show, Eq, Ord)

data TraceEntry = TraceEntry
  { op :: TraceOp
  , path :: FilePath
  } deriving (Show, Eq, Ord)

checkFsatrace :: IO ()
checkFsatrace = do
  m <- findExecutable "fsatrace"
  when (isNothing m) $
    bail "fsatrace is not installed. Install it from https://github.com/jacereda/fsatrace and ensure it is on your PATH."

wrapWithFsatrace :: FilePath -> String -> [String] -> (String, [String])
wrapWithFsatrace traceFile cmd args =
  ("fsatrace", ["rwmd", traceFile, "--", cmd] ++ args)

parseTraceOutput :: Text -> [TraceEntry]
parseTraceOutput content =
  mapMaybe parseLine (Text.lines content)
  where
    parseLine line =
      case Text.uncons line of
        Just (opChar, rest) | Text.isPrefixOf "|" rest ->
          case charToOp opChar of
            Just op -> Just TraceEntry { op, path = toString (Text.drop 1 rest) }
            Nothing -> Nothing
        _ -> Nothing

    charToOp 'r' = Just TraceRead
    charToOp 'w' = Just TraceWrite
    charToOp 'm' = Just TraceMove
    charToOp 'd' = Just TraceDelete
    charToOp 't' = Just TraceTouch
    charToOp 'q' = Just TraceQuery
    charToOp _   = Nothing

filterTraceEntries :: FilePath -> [TraceEntry] -> [TraceEntry]
filterTraceEntries rootDir entries =
  nub $ filter isProjectFile entries
  where
    allSystemPrefixes = ["/usr", "/lib", "/lib64", "/etc", "/proc", "/dev", "/sys", "/tmp", "/nix", "/var"]
    -- Don't exclude system prefixes that are ancestors of the root directory
    systemPrefixes = filter (\sp -> not (sp `isPrefixOf` rootDir)) allSystemPrefixes

    isProjectFile entry =
      let p = entry.path
          rel = makeRelative rootDir p
       in rootDir `isPrefixOf` p
          && isRelative rel
          && not (any (`isPrefixOf` p) systemPrefixes)

formatTraceReport :: FilePath -> [TraceEntry] -> Text
formatTraceReport rootDir entries =
  let reads_  = nub $ sort [makeRelative rootDir e.path | e <- entries, e.op == TraceRead]
      writes = nub $ sort [makeRelative rootDir e.path | e <- entries, e.op == TraceWrite]

      section :: Text -> [FilePath] -> Text
      section _     []    = ""
      section title paths = title <> "\n" <> Text.unlines (map (\p -> "  " <> toText p) paths)

   in "\n=== File System Trace Report ===\n\n"
      <> section "Files read:" reads_
      <> (if not (null reads_) && not (null writes) then "\n" else "")
      <> section "Files written:" writes
