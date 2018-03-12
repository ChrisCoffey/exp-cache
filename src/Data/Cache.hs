module Data.Cache (
    Cache,
    newCache,

    EvictionStrategy(..),

    -- The sequential LRU implementation
    SeqLRU,
    newSeqLRU
    ) where

import Data.Cache.Eviction (EvictionStrategy(..))
import Data.Cache.Eviction.LRU

import qualified Data.HashMap.Strict as HM
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

data Cache k v s =
    Cache {
        cacheData :: HM.HashMap k v,
        evictionStrategy :: s,
        maxSize :: Int
        }

newCache :: (Hashable k, NFData v, EvictionStrategy s k, Eq k, Ord k) =>
    Int -- ^ The maximum cache size
    -> s -- ^ The evictionStrategy
    -> Cache k v s
newCache maxSize evictionStrategy =
    Cache {
        cacheData = HM.empty,
        evictionStrategy,
        maxSize
    }
