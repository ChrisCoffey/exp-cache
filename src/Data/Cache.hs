module Data.Cache (
    Cache,
    newCache,

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
    newRR
    ) where

import Data.Cache.Eviction (EvictionStrategy(..))
import Data.Cache.Eviction.LRU
import Data.Cache.Eviction.MRU
import Data.Cache.Eviction.RR

import qualified Data.HashMap.Strict as HM
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

data Cache k v s =
    Cache {
        cacheData :: HM.HashMap k v,
        evictionStrategy :: s,
        maxSize :: Int,
        currentSize :: Int
        }

newCache :: (Hashable k, NFData v, EvictionStrategy s, Eq k, Ord k) =>
    Int -- ^ The maximum cache size
    -> s k -- ^ The evictionStrategy
    -> Cache k v (s k)
newCache maxSize evictionStrategy =
    Cache {
        cacheData = HM.empty,
        evictionStrategy,
        maxSize,
        currentSize = 0
    }

readThrough :: (Hashable k, NFData v, EvictionStrategy s, Eq k, Ord k, Monad m) =>
    Cache k v (s k)
    -> k
    -> (k -> m v)
    -> m (v , Cache k v (s k))
readThrough cache@(Cache {maxSize, evictionStrategy, cacheData, currentSize}) key onMiss =
    case HM.lookup key cacheData of
        -- A hit. Because the value is already in the cache, no need to evict. Update the
        -- accessed time for 'key'
        Just v -> do
            let strat' = recordLookup key evictionStrategy
            pure (v, cache {evictionStrategy = strat'} )
        -- On a miss when the cache is full:
        -- 1) evict the relevant key (removes from HashMap & Strategy)
        -- 2) Record the newest key in the strategy
        -- 3) Add key to the cache data
        Nothing | maxSize == currentSize -> do
            v <- onMiss key
            let (strat', evicted) = evict evictionStrategy
                strat'' = recordLookup key strat'
                cacheData' = HM.insert key v $ maybe cacheData (`HM.delete` cacheData) evicted
            pure (v, cache {cacheData = cacheData', evictionStrategy = strat''})
        -- A miss when the cache is not full
        -- 1) Record the new key in the data cache
        -- 2) Record the new key in the strategy
        Nothing -> do
            v <- onMiss key
            let strat' = recordLookup key evictionStrategy
                cacheData' = HM.insert key v cacheData
            pure (v, cache {cacheData = cacheData', evictionStrategy = strat'})
