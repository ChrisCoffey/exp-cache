module Data.Cache.Eviction.LRU (
    SeqLRU,
    newSeqLRU,

    LRU,
    newLRU,
    LRUContentsOnlyEq(..)
) where

import Data.Cache.Eviction

import Data.Sequence
import Data.Monoid ((<>))
import Data.Hashable (Hashable, hash)
import Data.Maybe (maybe)
import Data.Word (Word64)
import qualified Data.HashPSQ as PSQ


-- | This is a naive and terribly slow version of an LRU cache
newtype SeqLRU k = SeqLRU (Seq k)
    deriving (Eq, Show)

newSeqLRU :: SeqLRU k
newSeqLRU = SeqLRU empty

instance EvictionStrategy SeqLRU where
    recordLookup key (SeqLRU elements) =
        case viewl right of
            EmptyL -> SeqLRU $ elements |> key
            val :< rest -> SeqLRU $ (key <| left) <> right
        where
            (left, right) = breakl (== key) elements

    evict (SeqLRU elements) =
        case viewr elements of
            EmptyR -> (SeqLRU elements, Nothing)
            rest :> last -> (SeqLRU rest, Just last)

-- | An optimized version of an LRU cache
data LRU k =
    LRU {
        queue :: PSQ.HashPSQ k Word64 (),
        time :: Word64
        } deriving (Eq, Show)

newtype LRUContentsOnlyEq k = LRUContentsOnlyEq (LRU k)
    deriving Show
instance (Hashable k, Ord k) => Eq (LRUContentsOnlyEq k) where
    (==) (LRUContentsOnlyEq lru)
         (LRUContentsOnlyEq lru') = queue lru == queue lru'


newLRU :: LRU k
newLRU = LRU PSQ.empty 0

instance EvictionStrategy LRU where
    recordLookup key (LRU {time, queue} )
        | time == maxBound = let
            (newTime, queue') = shrinkPSQPriorities queue
            in recordLookup key $ LRU queue' newTime
        | otherwise = LRU  queue' (time + 1)
            where
                queue' = PSQ.insert key time () queue

    evict LRU {time, queue} =
        case PSQ.findMin queue of
            Just (evicted, _, _) -> (LRU queue' time, Just evicted)
            _ -> (LRU queue time, Nothing)
        where
            queue' = PSQ.deleteMin queue

-- | Transform the priorities of a PSQ by subtracting the minimum priority from all
-- priorities in the queue. This becomes necessary when reaching the upper bound on an
-- 'Int'. The ordering of priorities is retained
shrinkPSQPriorities :: (Integral p, Hashable k, Ord k) =>
    PSQ.HashPSQ k p v
    -> (p, PSQ.HashPSQ k p v)
shrinkPSQPriorities psq =
    PSQ.fold' reducePriority (0, PSQ.empty) psq
    where
        reducePriority k p v (maxValue, psq) = let
            newP = p - minValue
            m = max newP maxValue
            in (m, PSQ.insert k newP v psq)
        second (_, a, _) = a
        minValue = maybe 0 second $ PSQ.findMin psq
