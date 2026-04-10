module Trace
  ( checkFsatrace
  , wrapWithFsatrace
  , parseTraceOutput
  , TraceEntry(..)
  , TraceOp(..)
  , filterTraceEntries
  , formatFileReport
  , formatDirectoryReport
  , findDiscrepancies
  , formatDiscrepancies
  ) where

import Universum

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import System.Directory (findExecutable)
import System.FilePath (makeRelative, isRelative, (</>))
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

    -- Paths within the project that should be excluded (not meaningful inputs)
    excludedRelPrefixes = [".git/"]

    isProjectFile entry =
      let p = entry.path
          rel = makeRelative rootDir p
       in rootDir `isPrefixOf` p
          && isRelative rel
          && not (any (`isPrefixOf` p) systemPrefixes)
          && not (any (`isPrefixOf` rel) excludedRelPrefixes)

-- | Format trace report showing individual files (--trace-files)
formatFileReport :: FilePath -> [TraceEntry] -> Text
formatFileReport rootDir entries =
  let reads_  = nub $ sort [makeRelative rootDir e.path | e <- entries, e.op == TraceRead]
      writes = nub $ sort [makeRelative rootDir e.path | e <- entries, e.op == TraceWrite]

      section :: Text -> [FilePath] -> Text
      section _     []    = ""
      section title paths = title <> "\n" <> Text.unlines (map (\p -> "  " <> toText p) paths)

   in "\n=== File System Trace Report ===\n\n"
      <> section "Files read:" reads_
      <> (if not (null reads_) && not (null writes) then "\n" else "")
      <> section "Files written:" writes

-- | Format trace report showing directory-level summary (--trace, default)
formatDirectoryReport :: FilePath -> [TraceEntry] -> Text
formatDirectoryReport rootDir entries =
  let reads_  = nub [makeRelative rootDir e.path | e <- entries, e.op == TraceRead]
      writes = nub [makeRelative rootDir e.path | e <- entries, e.op == TraceWrite]

      dirSummary :: [FilePath] -> [(FilePath, Int)]
      dirSummary = sortOn fst . Map.toList . foldl' countDir Map.empty
        where
          countDir acc fp =
            let dir = topLevelDir fp
             in Map.insertWith (+) dir (1 :: Int) acc

      topLevelDir :: FilePath -> FilePath
      topLevelDir fp = case break (== '/') fp of
        (_, '/':_) -> takeWhile (/= '/') fp <> "/"
        _          -> fp  -- file at root level, show as-is

      section :: Text -> [(FilePath, Int)] -> Text
      section _     []    = ""
      section title dirs = title <> "\n" <> Text.unlines
        (map (\(d, n) -> "  " <> toText d <> " (" <> show n <> " files)") dirs)

   in "\n=== File System Trace Report ===\n\n"
      <> section "Directories read:" (dirSummary reads_)
      <> (if not (null reads_) && not (null writes) then "\n" else "")
      <> section "Directories written:" (dirSummary writes)

-- | Resolve snapshot input pathspecs to directories relative to rootDirectory.
-- Pathspecs: "." = cwd, ":/path" = from root, "relative" = relative to cwd
resolveInputPaths :: FilePath -> FilePath -> [FilePath] -> [FilePath]
resolveInputPaths rootDir cwd = map resolve
  where
    cwdRel = makeRelative rootDir cwd

    resolve (':':'/':rest) = rest           -- ":/libs/ps" -> "libs/ps"
    resolve "."            = cwdRel         -- "." -> cwd relative to root
    resolve p
      | "/" `isPrefixOf` p = makeRelative rootDir p  -- absolute path
      | otherwise          = cwdRel </> p            -- relative to cwd

-- | Check if a file path is covered by any of the resolved input directories.
isCoveredBy :: FilePath -> [FilePath] -> Bool
isCoveredBy file inputs = any covers inputs
  where
    covers "."    = True  -- "." means repo root, covers everything
    covers input
      | input == file = True
      | otherwise     = (input <> "/") `isPrefixOf` file

-- | Find files that were read but not covered by declared snapshot inputs.
findDiscrepancies :: FilePath -> FilePath -> [FilePath] -> [TraceEntry] -> [FilePath]
findDiscrepancies rootDir cwd snapshotInputs entries =
  let resolvedInputs = resolveInputPaths rootDir cwd snapshotInputs
      reads_ = nub $ sort [makeRelative rootDir e.path | e <- entries, e.op == TraceRead]
      -- Exclude the scripts themselves and taskrunner internals
      excludePrefixes = [".taskrunner/", "scripts/"]
   in filter (\f -> not (isCoveredBy f resolvedInputs)
                    && not (any (`isPrefixOf` f) excludePrefixes))
             reads_

-- | Format discrepancy warnings
formatDiscrepancies :: [FilePath] -> Text
formatDiscrepancies files =
  let dirSummary = sortOn fst . Map.toList . foldl' countDir Map.empty $ files
        where
          countDir acc fp =
            let dir = case break (== '/') fp of
                        (_, '/':_) -> takeWhile (/= '/') fp <> "/"
                        _          -> fp
             in Map.insertWith (+) dir (1 :: Int) acc

   in "\n=== Snapshot Discrepancies ===\n"
      <> "Files read but NOT covered by snapshot inputs:\n"
      <> Text.unlines (map (\(d, n) -> "  " <> toText d <> " (" <> show n <> " files)") dirSummary)
      <> "\n" <> Text.unlines (map (\f -> "  " <> toText f) files)
