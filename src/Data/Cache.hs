module Data.Cache (
    Cache,
    newCache
    ) where

import Data.Cache.Eviction (EvictionStrategy)

import qualified Data.HashMap.Strict as HM
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

data Cache k v s =
    Cache {
        cacheData :: HM.HashMap k v,
        evictionStrategy :: s,
        maxSize :: Int
        }

newCache :: (Hashable k, NFData v, EvictionStrategy s k) =>
    Int -- ^ The maximum cache size
    -> s -- ^ The evictionStrategy
    -> Cache k v s
newCache maxSize evictionStrategy =
    Cache {
        cacheData = HM.empty,
        evictionStrategy,
        maxSize
    }
