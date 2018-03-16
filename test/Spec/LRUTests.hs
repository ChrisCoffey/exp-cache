module Spec.LRUTests (
    lruTests
) where

import Data.Cache
import Data.Cache.Eviction.LRU

import           Test.HUnit                   ((@=?))
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (testCase)


lruTests :: TestTree
lruTests = testGroup "LRU" [
    seqLRUTests,
    psqLRUTests
    ]


seqLRUTests :: TestTree
seqLRUTests = testGroup "Sequential" [
    testCase "evict empty == empty" $ do
        let strat = newSeqLRU :: SeqLRU Int
            (s, Nothing) = evict strat :: (SeqLRU Int, Maybe Int)
        s @=? strat
    , testCase "evict (1:empty) == (empty, Just 1) " $ do
        let strat = newSeqLRU :: SeqLRU Int
            s = recordLookup (1 :: Int) strat
            (s', Just _) = evict s :: (SeqLRU Int, Maybe Int)
        s' @=? strat
    , testCase "read a b a => evict b a" $ do
        let strat = newSeqLRU :: SeqLRU Int
            [x,y] = [1,2] :: [Int]
            s = recordLookup x . recordLookup y $ recordLookup x strat
            (s', Just a) = evict s
            (s'', Just b) = evict s'
            (_, Nothing) = evict s'' :: (SeqLRU Int, Maybe Int)
        a == y @=? True
        b == x @=? True
    , testCase "read a b c => evict a b c" $ do
        let strat = newSeqLRU :: SeqLRU Int
            [x,y,z] = [1,2,3] :: [Int]
            s = recordLookup x . recordLookup y $ recordLookup z strat
            (s', Just a) = evict s
            (s'', Just b) = evict s'
            (_, Just c) = evict s'' :: (SeqLRU Int, Maybe Int)
        a @=? x
        b @=? y
        c @=? z
    ]


psqLRUTests :: TestTree
psqLRUTests = testGroup "Priority queue" [
    testCase "evict empty == empty" $ do
        let strat = newLRU :: LRU Int
            (s, Nothing) = evict strat :: (LRU Int, Maybe Int)
        s @=? strat
    , testCase "evict (1:empty) == (empty, Just 1) " $ do
        let strat = newLRU :: LRU Int
            s = recordLookup (1 :: Int) strat
            (s', Just _) = evict s :: (LRU Int, Maybe Int)
        LRUContentsOnlyEq s' @=? LRUContentsOnlyEq strat
    , testCase "read a b a => evict b a" $ do
        let strat = newLRU :: LRU Int
            [x,y] = [1,2] :: [Int]
            s = recordLookup x . recordLookup y $ recordLookup x strat
            (s', Just a) = evict s
            (s'', Just b) = evict s'
            (_, Nothing) = evict s'' :: (LRU Int, Maybe Int)
        a == y @=? True
        b == x @=? True
    , testCase "read a b c => evict a b c" $ do
        let strat = newLRU :: LRU Int
            [x,y,z] = [1,2,3] :: [Int]
            s = recordLookup x . recordLookup y $ recordLookup z strat
            (s', Just a) = evict s
            (s'', Just b) = evict s'
            (_, Just c) = evict s'' :: (LRU Int, Maybe Int)
        a @=? z
        b @=? y
        c @=? x
    ]
