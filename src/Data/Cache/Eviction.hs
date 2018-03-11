module Data.Cache.Eviction (
    EvictionStrategy(..)
) where

import Data.Hashable (Hashable)
import Data.Proxy (Proxy)

class EvictionStrategy s k where
    recordLookup :: Hashable k =>
        k -- ^ The key to lookup
        -> s -- ^ The strategy (containing any state necessary)
        -> s -- ^ The strategy + state folloiwng adding the key

    evict ::
        Proxy k
        -> s
        -> s
