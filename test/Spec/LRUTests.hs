module Spec.LRUTests (
    lruTests
) where

import Data.Cache

import           Test.HUnit                   ((@=?))
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (testCase)


lruTests :: TestTree
lruTests = testGroup "LRU" [
    seqLRUTests
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

