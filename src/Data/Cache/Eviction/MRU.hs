module Data.Cache.Eviction.MRU (
    MRU,
    newMRU,
    MRUContentsOnlyEq(..)
) where

import Data.Cache.Eviction

import Data.Sequence
import Data.Monoid ((<>))
import Data.Hashable (Hashable, hash)
import Data.Maybe (maybe)
import Data.Word (Word64)
import qualified Data.HashPSQ as PSQ

data MRU k =
    MRU {
        queue :: PSQ.HashPSQ k Word64 (),
        time :: Word64
        } deriving (Eq, Show)

newtype MRUContentsOnlyEq k = MRUContentsOnlyEq (MRU k)
    deriving Show
instance (Hashable k, Ord k) => Eq (MRUContentsOnlyEq k) where
    (==) (MRUContentsOnlyEq lru)
         (MRUContentsOnlyEq lru') = queue lru == queue lru'


newMRU :: MRU k
newMRU = MRU PSQ.empty maxBound

instance EvictionStrategy MRU where
    recordLookup key (MRU {time, queue} )
        | time == minBound = let
            (newTime, queue') = expandPSQPriorities queue
            in recordLookup key $ MRU queue' newTime
        | otherwise = MRU  queue' (time - 1)
            where
                queue' = PSQ.insert key time () queue

    evict MRU {time, queue} =
        case PSQ.findMin queue of
            Just (evicted, _, _) -> (MRU queue' time, Just evicted)
            _ -> (MRU queue time, Nothing)
        where
            queue' = PSQ.deleteMin queue

--TODO unify with LRU implementation
expandPSQPriorities :: (Integral p, Hashable k, Ord k, Bounded p) =>
    PSQ.HashPSQ k p v
    -> (p, PSQ.HashPSQ k p v)
expandPSQPriorities psq =
    PSQ.fold' increasePriority (maxValue, PSQ.empty) psq
    where
        increasePriority k p v (minValue, psq) = let
            -- Sutract the difference between p and largest p from upper bound
            newP = maxBound - (maxValue - p)
            m = min newP maxValue
            in (m, PSQ.insert k newP v psq)
        second (_, a, _) = a
        maxValue = maybe maxBound ((maxBound -). second) $ PSQ.findMin psq
