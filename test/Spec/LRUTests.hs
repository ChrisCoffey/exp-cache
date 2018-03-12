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

    ]

