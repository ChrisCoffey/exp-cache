module Data.Cache.Eviction.LRU (
    SeqLRU,
    newSeqLRU,

    LRU,
    newLRU
) where

import Data.Cache.Eviction

import Data.Sequence
import Data.Monoid ((<>))
import Data.Hashable (Hashable, hash)
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
        queue :: PSQ.HashPSQ k Integer (),
        time :: Integer
        } deriving (Eq, Show)

newLRU :: LRU k
newLRU = LRU PSQ.empty 0

instance EvictionStrategy LRU where
    recordLookup key (LRU {time, queue} ) =
        LRU  queue' (time + 1)
        where
            queue' = PSQ.insert key time () queue

    evict LRU {time, queue} =
        case PSQ.findMin queue of
            Just (evicted, _, _) -> (LRU queue' time, Just evicted)
            _ -> (LRU queue time, Nothing)
        where
            queue' = PSQ.deleteMin queue
