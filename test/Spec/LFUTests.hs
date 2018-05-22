module Spec.LFUTests (
    lfuTests
) where

import Data.Cache
import Data.Cache.Eviction.LFU

import           Test.HUnit                   ((@=?))
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (testCase)


lfuTests :: TestTree
lfuTests = testGroup "Least Frequently Used" [
    testCase "evict empty == empty" $ do
        let strat = newLFU :: LFU Int
            (s, Nothing) = evict strat :: (LFU Int, Maybe Int)
        s @=? strat
    , testCase "evict (1:empty) == (empty, Just 1) " $ do
        let strat = newLFU :: LFU Int
            s = recordLookup (1 :: Int) strat
            (s', Just _) = evict s :: (LFU Int, Maybe Int)
        LFUContentsOnlyEq s' @=? LFUContentsOnlyEq strat
    , testCase "read a b a => evict b a" $ do
        let strat = newLFU :: LFU Int
            [x,y] = [1,2] :: [Int]
            s = recordLookup x . recordLookup y $ recordLookup x strat
            (s', Just a) = evict s
            (s'', Just b) = evict s'
            (_, Nothing) = evict s'' :: (LFU Int, Maybe Int)
        a == y @=? True
        b == x @=? True
    , testCase "read a b c c b => evict a" $ do
        let strat = newLFU :: LFU Int
            [v,w,x,y,z] = [1,2,3,3,2] :: [Int]
            s = recordLookup v . recordLookup w . recordLookup x . recordLookup y $ recordLookup z strat
            (s', Just a) = evict s:: (LFU Int, Maybe Int)
        a @=? v
    ]
