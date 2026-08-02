{-# LANGUAGE BangPatterns #-}
-- | Extracting a cache bundle is dominated by per-file filesystem work, not by
-- the download or by decompression: on CI a 80 MiB bundle holding ~57k mostly
-- tiny files takes ~0.5s to download and ~23s to unpack. That cost is latency
-- per entry, so it parallelises well.
--
-- This module splits a tar stream across several concurrent @tar -x@ processes.
-- We parse only enough of each header to find where the entry ends; the bytes
-- themselves are forwarded verbatim, so permissions, mtimes, symlinks, long
-- names and every other GNU tar semantic are still handled by real tar. The
-- archive format is untouched, so existing cache bundles work unchanged.
module ParallelUnpack (unpackTarParallel) where

import Universum

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Conduit (ConduitT, bracketP)
import qualified Data.Conduit as C
import Data.List ((!!))
import qualified Data.Set as Set
import Control.Monad.Trans.Resource (MonadResource)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hSetBuffering, BufferMode (BlockBuffering))
import System.Process (CreateProcess (..), StdStream (..), cleanupProcess, createProcess_, proc, waitForProcess)
import Types
import Utils (bail, formatSeconds, logDebug, timed)

-- | Everything in a tar archive is a multiple of this.
blockSize :: Int
blockSize = 512

-- | A tar header, parsed just far enough to find the next one. The name stays
-- as raw bytes: it is only needed to create parent directories, which is rare
-- compared to the number of entries.
data Header = Header
  { name :: BS.ByteString
  , size :: Int
  , typeflag :: Char
  }

-- | Extract a (decompressed) tar stream using @workerCount@ concurrent @tar -x@
-- processes, all with cwd @workdir@.
unpackTarParallel :: MonadResource m => AppState -> Handle -> FilePath -> Int -> ConduitT BS.ByteString Void m ()
unpackTarParallel appState stderrHandle workdir workerCount = do
  liftIO $ logDebug appState $ "Running " <> show workerCount <> " subprocesses: "
    <> show (tarCmd:tarArgs) <> " in cwd " <> show workdir

  bracketP (replicateM workerCount startTar) (mapM_ cleanupProcess) \procs -> do
    pipes <- forM procs \case
      (Just stdinPipe, _, _, _) -> do
        -- Entries are mostly small, so the default buffer would turn this into
        -- one write syscall per few entries.
        liftIO $ hSetBuffering stdinPipe (BlockBuffering (Just pipeBufferSize))
        pure stdinPipe
      _ -> liftIO $ error "unable to obtain stdin pipe"

    -- Directories we have already created. Two tar processes racing to
    -- auto-create the same parent directory corrupts the tree (files end up
    -- missing), so the workers must never have to create one: we create every
    -- directory here, single-threaded, before dispatching the entry that needs
    -- it.
    madeRef <- newIORef Set.empty
    -- Directory and hardlink entries, replayed sequentially once the workers
    -- are done: a hardlink needs its target to exist, and directory metadata
    -- must be applied after the files inside them have been written. Both are
    -- header-only, so holding them costs 512 bytes per directory.
    deferredRef <- newIORef []
    entriesRef <- newIORef (0 :: Int)
    -- Entries arrive depth first, so consecutive ones nearly always share a
    -- parent directory; remembering the last one skips most set lookups.
    lastDirRef <- newIORef BS.empty

    let
      ensureDir dir = do
        lastDir <- readIORef lastDirRef
        unless (dir == lastDir || BS.null dir || dir == ".") do
          made <- readIORef madeRef
          unless (Set.member dir made) do
            createDirectoryIfMissing True (workdir </> decodePath dir)
            modifyIORef' madeRef (Set.insert dir)
          writeIORef lastDirRef dir

      -- @pending@ holds extended headers ('L', 'K', 'x'), which describe the
      -- entry that follows and so must reach the same worker as it.
      loop !next !pending !longName = do
        block <- readExactly blockSize
        if
          -- An archive ends with zero blocks. Testing the first byte first
          -- keeps the full scan off the path every real header takes.
          | BS.null block || (BS.head block == 0 && BS.all (== 0) block) ->
              pass
          | BS.length block < blockSize ->
              liftIO $ bail "truncated tar archive"
          | otherwise -> do
              header <- either (liftIO . bail) pure $ parseHeader block
              let entryName = fromMaybe header.name longName
                  padded = ((header.size + blockSize - 1) `div` blockSize) * blockSize
              case header.typeflag of
                -- Extended headers describe the entry that follows, so they
                -- have to reach the same worker as it.
                t | t == 'L' || t == 'K' || t == 'x' || t == 'g' -> do
                      body <- readExactly padded
                      let longName' = case t of
                            'L' -> Just (BS.takeWhile (/= 0) body)
                            'x' -> paxPath body <|> longName
                            _ -> longName
                      -- Newest first; reversed again when written out.
                      loop next (body : block : pending) longName'
                t | t == '5' || t == '1' -> do
                      body <- readExactly padded
                      when (t == '5') $ liftIO $ ensureDir (dropTrailingSlash entryName)
                      liftIO do
                        modifyIORef' deferredRef (BS.concat (reverse (body : block : pending)) :)
                        modifyIORef' entriesRef (+ 1)
                      loop next [] Nothing
                _ -> do
                      -- Round-robin: assignment stays balanced without a slow
                      -- worker attracting more work, which a least-loaded
                      -- policy would do once its pipe backs up.
                      let pipe = pipes !! (next `mod` workerCount)
                      liftIO do
                        ensureDir (parentDir entryName)
                        mapM_ (BS.hPut pipe) (reverse (block : pending))
                        modifyIORef' entriesRef (+ 1)
                      copyExactly pipe padded
                      loop (next + 1) [] Nothing

    loop (0 :: Int) [] Nothing

    liftIO do
      (_, drainSeconds) <- timed do
        forM_ pipes \pipe -> BS.hPut pipe endOfArchive >> hClose pipe
        forM_ procs \(_, _, _, process) -> checkExit =<< waitForProcess process

      deferred <- reverse <$> readIORef deferredRef
      (_, deferredSeconds) <- timed $ unless (null deferred) $ runDeferredPass deferred

      entries <- readIORef entriesRef
      -- No figure for the feed loop itself: it blocks whenever a worker's pipe
      -- is full, so its wall clock conflated this thread's work with waiting for
      -- tar. 'restoreCache' attributes that properly, via a tap either side of
      -- zstd.
      logDebug appState $ "Unpacked " <> show entries <> " entries using "
        <> show workerCount <> " tar processes (" <> show (length deferred)
        <> " directories and hardlinks in " <> formatSeconds deferredSeconds
        <> ", workers drained in " <> formatSeconds drainSeconds <> ")"
  where
  tarCmd = "tar"
  tarArgs = ["-x"]

  startTar = createProcess_ "createProcess_"
    (proc tarCmd tarArgs)
      { std_in = CreatePipe
      , std_err = UseHandle stderrHandle
      , cwd = Just workdir
      }

  -- One last tar for the entries that could not be extracted concurrently.
  runDeferredPass blocks =
    bracket startTar cleanupProcess \case
      (Just stdinPipe, _, _, process) -> do
        mapM_ (BS.hPut stdinPipe) blocks
        BS.hPut stdinPipe endOfArchive
        hClose stdinPipe
        checkExit =<< waitForProcess process
      _ ->
        error "unable to obtain stdin pipe"

  checkExit exitCode =
    when (exitCode /= ExitSuccess) do
      bail $ "tar unpack command failed with code: " <> show exitCode

-- | Big enough that a run of small entries becomes one write syscall.
pipeBufferSize :: Int
pipeBufferSize = 256 * 1024

-- | tar marks the end of an archive with two zero blocks. Without them tar
-- reports an unexpected EOF and exits nonzero.
endOfArchive :: BS.ByteString
endOfArchive = BS.replicate (2 * blockSize) 0

-- | Read exactly @n@ bytes, returning fewer only at end of input.
readExactly :: Monad m => Int -> ConduitT BS.ByteString o m BS.ByteString
readExactly = go []
  where
  -- Upstream chunks are far bigger than a header, so the usual case is one
  -- slice of the current chunk and no copying at all.
  join1 [] chunk = chunk
  join1 acc chunk = BS.concat (reverse (chunk : acc))

  go [] 0 = pure BS.empty
  go acc 0 = pure $ BS.concat (reverse acc)
  go acc n =
    C.await >>= \case
      Nothing -> pure $ BS.concat (reverse acc)
      Just chunk
        | BS.length chunk <= n -> go (chunk : acc) (n - BS.length chunk)
        | otherwise -> do
            let (wanted, rest) = BS.splitAt n chunk
            C.leftover rest
            pure $ join1 acc wanted

-- | Copy exactly @n@ bytes from upstream to a handle, without holding the whole
-- entry in memory - archives may contain individually huge files.
copyExactly :: MonadIO m => Handle -> Int -> ConduitT BS.ByteString o m ()
copyExactly pipe = go
  where
  go 0 = pass
  go n =
    C.await >>= \case
      Nothing -> liftIO $ bail "truncated tar archive"
      Just chunk
        | BS.length chunk <= n -> do
            liftIO $ BS.hPut pipe chunk
            go (n - BS.length chunk)
        | otherwise -> do
            let (wanted, rest) = BS.splitAt n chunk
            liftIO $ BS.hPut pipe wanted
            C.leftover rest

parseHeader :: BS.ByteString -> Either String Header
parseHeader block = do
  unless (checksumMatches block) $
    Left "tar header checksum mismatch - corrupt archive?"
  size <- maybeToRight "invalid size field in tar header" $ parseNumeric (field 124 12)
  pure Header
    { name = fullName
    , size
    , typeflag = B8.head (field 156 1)
    }
  where
  field offset len = BS.take len (BS.drop offset block)
  nulTerminated = BS.takeWhile (/= 0)

  -- The prefix field only holds part of the path in POSIX ustar archives; in
  -- the GNU format those bytes mean something else entirely, and long names
  -- arrive as a separate 'L' entry instead.
  name_ = nulTerminated (field 0 100)
  prefix = nulTerminated (field 345 155)
  fullName
    | field 257 6 == "ustar\0", not (BS.null prefix) = prefix <> "/" <> name_
    | otherwise = name_

-- | Guards against silently desynchronising from the stream: if a size field
-- were misread we would slice the archive at the wrong offset and hand every
-- worker garbage.
--
-- This runs on every header, so it sums the bytes in place rather than
-- materialising a blanked-out copy of the block.
checksumMatches :: BS.ByteString -> Bool
checksumMatches block =
  case parseNumeric checksumField of
    Nothing -> False
    -- The checksum is defined over the header with its own field read as
    -- spaces. Some historic tars summed the bytes as signed, so accept either.
    Just expected -> expected == unsignedSum || expected == signedSum
  where
  checksumField = BS.take 8 (BS.drop 148 block)
  blankedOut = 8 * 32
  sumWith f bs = BS.foldl' (\acc byte -> acc + f byte) (0 :: Int) bs
  unsigned = fromIntegral :: Word8 -> Int
  signed byte = if byte > 127 then fromIntegral byte - 256 else fromIntegral byte
  unsignedSum = sumWith unsigned block - sumWith unsigned checksumField + blankedOut
  signedSum = sumWith signed block - sumWith signed checksumField + blankedOut

-- | Header numbers are octal, except that large values use a base-256 escape
-- flagged by the top bit of the first byte.
parseNumeric :: BS.ByteString -> Maybe Int
parseNumeric bs
  | BS.null bs = Just 0
  | BS.head bs .&. 0x80 /= 0 = Just $ BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 (BS.tail bs)
  | BS.null digits = Just 0
  | otherwise = Just $ BS.foldl' (\acc c -> acc * 8 + fromIntegral (c - 0x30)) 0 digits
  where
  digits = BS.takeWhile (\c -> 0x30 <= c && c <= 0x37) (BS.dropWhile (== 0x20) bs)

-- | The @path@ record of a PAX extended header, which overrides the name in the
-- header that follows. Records look like @"<len> <key>=<value>\n"@, where
-- @len@ counts the whole record.
paxPath :: BS.ByteString -> Maybe BS.ByteString
paxPath body = find (const True) [ value | Just value <- map (BS.stripPrefix "path=") (records body) ]
  where
  records bs = case B8.readInt bs of
    Just (len, afterDigits)
      | len > 0, len <= BS.length bs, contentLen >= 0 ->
          BS.take contentLen (BS.drop 1 afterDigits) : records (BS.drop len bs)
      where
      -- Drop the length digits, the separating space and the trailing newline.
      contentLen = len - (BS.length bs - BS.length afterDigits) - 2
    _ ->
      []

-- | Names are only used to create parent directories; tar itself receives the
-- raw bytes, so a name we cannot decode does not corrupt the extracted file.
decodePath :: BS.ByteString -> FilePath
decodePath = toString . decodeUtf8 @Text

-- | Everything up to the last @/@, i.e. the directory the entry lives in.
-- Empty for a top-level entry. Written the same way as a directory entry's own
-- name so both hit the same cache.
parentDir :: BS.ByteString -> BS.ByteString
parentDir = dropTrailingSlash . fst . B8.breakEnd (== '/')

dropTrailingSlash :: BS.ByteString -> BS.ByteString
dropTrailingSlash path
  | not (BS.null path), B8.last path == '/' = BS.init path
  | otherwise = path
