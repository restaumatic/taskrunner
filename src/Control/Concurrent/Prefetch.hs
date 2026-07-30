-- | Fetch a list of items concurrently, but consume the results strictly in
-- order. Useful for turning a sequence of independent, latency-bound requests
-- into a stream without buffering everything in memory at once.
module Control.Concurrent.Prefetch
  ( Prefetch
  , startPrefetch
  , cancelPrefetch
  , nextPrefetch
  ) where

import Universum

import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Concurrent.STM (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (mask_)
import Data.List (delete)

data Prefetch a = Prefetch
  { queue :: TBQueue (Maybe (Async a))
    -- ^ Results in item order. 'Nothing' marks the end of the stream.
  , producer :: Async ()
  , inFlight :: TVar [Async a]
    -- ^ Fetches that have been started but not yet consumed, so that
    -- 'cancelPrefetch' can stop them. Consumed fetches are removed, otherwise
    -- we would keep every result alive until the whole stream is done.
  }

-- | Start fetching @items@ in the background, at most @concurrency + 1@ at a
-- time, and hand them out in order via 'nextPrefetch'.
--
-- Memory use is bounded by the size of @concurrency + 1@ results, since a fetch
-- is only started once there is room for its result.
--
-- Must be paired with 'cancelPrefetch' (via 'bracket' or similar), which is
-- also what makes exceptions safe: if a fetch fails, 'nextPrefetch' rethrows it
-- and 'cancelPrefetch' stops the remaining ones.
startPrefetch :: Int -> [i] -> (i -> IO a) -> IO (Prefetch a)
startPrefetch concurrency items fetch = do
  queue <- newTBQueueIO (fromIntegral (max 1 concurrency))
  inFlight <- newTVarIO []
  producer <- async do
    forM_ items \item -> do
      -- Registering the fetch must not be interruptible, or a cancellation
      -- landing in between would leave an unreachable thread running.
      a <- mask_ do
        a <- async (fetch item)
        atomically $ modifyTVar' inFlight (a:)
        pure a
      -- Blocks while the consumer is behind, which is what bounds concurrency.
      atomically $ writeTBQueue queue (Just a)
    atomically $ writeTBQueue queue Nothing
  pure Prefetch{queue, producer, inFlight}

-- | Stop the producer and any outstanding fetches. Idempotent.
--
-- Note that this makes any concurrent 'nextPrefetch' block forever, so only
-- call it once the consumer is done with the stream.
cancelPrefetch :: Prefetch a -> IO ()
cancelPrefetch prefetch = do
  -- Cancel the producer first, so that it cannot start anything new while we
  -- are cancelling what is already in flight.
  cancel prefetch.producer
  readTVarIO prefetch.inFlight >>= mapM_ cancel

-- | Next result in item order, or 'Nothing' once all items have been handed
-- out. Rethrows whatever the corresponding fetch threw.
nextPrefetch :: Prefetch a -> IO (Maybe a)
nextPrefetch prefetch =
  atomically (readTBQueue prefetch.queue) >>= \case
    Nothing ->
      pure Nothing
    Just a ->
      Just <$> wait a `finally` atomically (modifyTVar' prefetch.inFlight (delete a))
