module Data.Cache.Eviction.LFU (
    LFU,
    newLFU,
    -- | For testing
    LFUContentsOnlyEq(..)
) where

import Data.Cache.Eviction
import Data.Word (Word64)
import Data.Hashable
import qualified Data.HashPSQ as PSQ

newtype LFU k = LFU {
    queue :: PSQ.HashPSQ k Word64 ()
    } deriving (Eq, Show)

newtype LFUContentsOnlyEq k = LFUContentsOnlyEq (LFU k)
    deriving Show
instance ( Hashable k, Ord k ) => Eq (LFUContentsOnlyEq k) where
    (==) (LFUContentsOnlyEq lfu)
         (LFUContentsOnlyEq lfu') = queue lfu == queue lfu'

newLFU :: LFU k
newLFU = LFU PSQ.empty

instance EvictionStrategy LFU where
    recordLookup key (LFU queue) =
        LFU . snd $ PSQ.alter repsert key queue
        where
            repsert Nothing = ((), Just (1, ()) )
            repsert (Just (count, x)) = ((), Just (count + 1, x))

    evict (LFU queue) =
        case PSQ.findMin queue of
            Just (evicted, _, _) -> (LFU queue', Just evicted)
            Nothing -> (LFU queue, Nothing)
        where
            queue' = PSQ.deleteMin queue
