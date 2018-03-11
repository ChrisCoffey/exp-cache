module Data.Cache.Eviction.LRU (
    SeqLRU
) where

import Data.Cache.Eviction

import Data.Sequence
import Data.Monoid ((<>))
import Data.Hashable (Hashable, hash)
import Data.Proxy (Proxy)

newtype SeqLRU = SeqLRU (Seq Int)

instance (Hashable k) => EvictionStrategy SeqLRU k where
    recordLookup :: (Hashable k) => k -> SeqLRU -> SeqLRU
    recordLookup k (SeqLRU elements) =
        case viewl right of
            EmptyL -> SeqLRU $ elements |> key
            val :< rest -> SeqLRU $ (key <| left) <> right
        where
            (left, right) = breakl (== key) elements
            key = hash k

    evict :: Proxy k -> SeqLRU -> SeqLRU
    evict _ (SeqLRU elements) =
        case viewr elements of
            EmptyR -> SeqLRU elements
            rest :> last -> SeqLRU rest
