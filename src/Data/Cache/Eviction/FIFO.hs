module Data.Cache.Eviction.FIFO (
    FIFO,
    newFIFO
) where

import Data.Cache.Eviction
import Data.Sequence

-- | A First in First Out eviction strategy. Items are evicted based on order added, regardless of usage patterns.
newtype FIFO k = FIFO (Seq k)
    deriving (Eq, Show)

newFIFO :: FIFO k
newFIFO = FIFO empty

instance EvictionStrategy FIFO where
    recordLookup key (FIFO keys) =
        FIFO (key <| keys)

    evict (FIFO keys) =
        case viewr keys of
            EmptyR -> (FIFO keys, Nothing)
            rest :> last -> (FIFO $ filter (/= last) rest, Just last)
