module Data.Cache (
    Cache,
    newCache,
    readThrough,

    EvictionStrategy(..),

    -- The sequential LRU implementation
    SeqLRU,
    newSeqLRU,
    -- Actual LRU implementation
    LRU,
    newLRU,

    -- An MRU implementation based on the LRU implementation
    MRU,
    newMRU,

    -- Random Replacement cache (RR)
    RR,
    newRR,

    -- | Least Frequently Used Cache
    LFU,
    newLFU,

    -- | First in First Out Cache (a queue)
    FIFO,
    newFIFO
    ) where

import Data.Cache.Eviction (EvictionStrategy(..))
import Data.Cache.Eviction.LRU
import Data.Cache.Eviction.MRU
import Data.Cache.Eviction.RR
import Data.Cache.Eviction.LFU
import Data.Cache.Eviction.FIFO

import qualified Data.HashMap.Strict as HM
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Numeric.Natural

-- | A Cache is a bounded collection of data that supports fast random access. When the cach is full, the associated
-- 'EvictionStrategy' is used to discard an item from the cache. Library users should use 'readThrough' to read and
-- replace items in the cache.
data Cache k v s =
    Cache {
        cacheData :: HM.HashMap k v,
        evictionStrategy :: s,
        maxSize :: !Int,
        currentSize :: !Int
        }

-- | Create a new cache of the specified size using the provided 'EvictionStrategy'
newCache :: (Hashable k, NFData v, EvictionStrategy s, Eq k, Ord k) =>
    Natural -- ^ The maximum cache size
    -> s k -- ^ The evictionStrategy
    -> Cache k v (s k)
newCache 0 _ = error "Invalid cache size"
newCache maxSize evictionStrategy =
    Cache {
        cacheData = HM.empty,
        evictionStrategy,
        maxSize = fromIntegral maxSize,
        currentSize = 0
    }

-- | Performs a read-through operation on a given cache.
readThrough :: (Hashable k, NFData v, EvictionStrategy s, Eq k, Ord k, Monad m) =>
    Cache k v (s k) -- ^ The current cache state
    -> k            -- ^ The key to look up in the cache
    -> (k -> m v)   -- ^ The accessor function to evaluate if the key is not found
    -> m (v , Cache k v (s k))
readThrough cache@(Cache {maxSize, cacheData, currentSize}) key onMiss =
    case HM.lookup key cacheData of
        -- On a miss when the cache is full:
        -- 1) evict the relevant key (removes from HashMap & Strategy)
        -- 2) Record the newest key in the strategy
        -- 3) Add key to the cache data
        Nothing | maxSize <= currentSize -> do
            v <- onMiss key
            let cache' = postEviction v
            pure (v, cache' {currentSize = currentSize + 1})
        -- A hit. Because the value is already in the cache, no need to evaluate onMiss. Update the
        -- accessed time for 'key'
        Just v -> do
            let cache' = if maxSize <= currentSize
                     then postEviction v
                     else cache
                strat' = recordLookup key (evictionStrategy cache')
            pure (v, cache' {evictionStrategy = strat'} )
        -- A miss when the cache is not full
        -- 1) Record the new key in the data cache
        -- 2) Record the new key in the strategy
        Nothing -> do
            v <- onMiss key
            let strat' = recordLookup key (evictionStrategy cache)
                cacheData' = HM.insert key v cacheData
            pure (v, cache {cacheData = cacheData', evictionStrategy = strat', currentSize = currentSize + 1})
        where
            postEviction v = let
                (strat', evicted) = evict (evictionStrategy cache)
                strat'' = recordLookup key strat'
                cacheData' = HM.insert key v $ maybe cacheData (`HM.delete` cacheData) evicted
                in cache {cacheData = cacheData', evictionStrategy = strat''}
