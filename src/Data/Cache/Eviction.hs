module Data.Cache.Eviction (
    EvictionStrategy(..)
) where

import Data.Hashable (Hashable)

class EvictionStrategy s k where
    recordLookup :: Eq k =>
        k -- ^ The key to lookup
        -> s -- ^ The strategy (containing any state necessary)
        -> s -- ^ The strategy + state folloiwng adding the key

    evict ::
        s
        -> (s, Maybe k)
