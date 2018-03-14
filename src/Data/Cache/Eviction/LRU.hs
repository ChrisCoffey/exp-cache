module Data.Cache.Eviction.LRU (
    SeqLRU,
    newSeqLRU
) where

import Data.Cache.Eviction

import Data.Sequence
import Data.Monoid ((<>))
import Data.Hashable (Hashable, hash)

newtype SeqLRU k = SeqLRU (Seq k)
    deriving (Eq, Show)

newSeqLRU :: SeqLRU k
newSeqLRU = SeqLRU empty

instance EvictionStrategy SeqLRU where
    recordLookup :: (Eq k) => k -> SeqLRU k -> SeqLRU k
    recordLookup key (SeqLRU elements) =
        case viewl right of
            EmptyL -> SeqLRU $ elements |> key
            val :< rest -> SeqLRU $ (key <| left) <> right
        where
            (left, right) = breakl (== key) elements

    evict :: SeqLRU k -> (SeqLRU k, Maybe k)
    evict (SeqLRU elements) =
        case viewr elements of
            EmptyR -> (SeqLRU elements, Nothing)
            rest :> last -> (SeqLRU rest, Just last)
