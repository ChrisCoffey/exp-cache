module Data.Cache.Eviction (
    EvictionStrategy(..)
) where

import Data.Hashable (Hashable)

class EvictionStrategy s where
    recordLookup :: (Eq k, Hashable k, Ord k) =>
        k -- ^ The key to lookup
        -> s k-- ^ The strategy (containing any state necessary)
        -> s k -- ^ The strategy + state folloiwng adding the key

    evict :: (Eq k, Hashable k, Ord k) =>
        s k
        -> (s k, Maybe k)
