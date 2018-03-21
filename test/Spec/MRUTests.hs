module  Spec.MRUTests (
    mruTests
) where

import Data.Cache
import Data.Cache.Eviction.MRU

import           Test.HUnit                   ((@=?))
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (testCase)


mruTests :: TestTree
mruTests = testGroup "mru" [
    testCase "evict empty == empty" $ do
        let strat = newMRU :: MRU Int
            (s, Nothing) = evict strat :: (MRU Int, Maybe Int)
        s @=? strat
    , testCase "evict (1:empty) == (empty, Just 1) " $ do
        let strat = newMRU :: MRU Int
            s = recordLookup (1 :: Int) strat
            (s', Just _) = evict s :: (MRU Int, Maybe Int)
        MRUContentsOnlyEq s' @=? MRUContentsOnlyEq strat
    , testCase "read a b a => evict a b" $ do
        let strat = newMRU :: MRU Int
            [x,y] = [1,2] :: [Int]
            s = recordLookup x . recordLookup y $ recordLookup x strat
            (s', Just a) = evict s
            (s'', Just b) = evict s'
            (_, Nothing) = evict s'' :: (MRU Int, Maybe Int)
        a == x @=? True
        b == y @=? True
    , testCase "read a b c => evict c b a" $ do
        let strat = newMRU :: MRU Int
            [x,y,z] = [1,2,3] :: [Int]
            s = recordLookup x . recordLookup y $ recordLookup z strat
            (s', Just a) = evict s
            (s'', Just b) = evict s'
            (_, Just c) = evict s'' :: (MRU Int, Maybe Int)
        a @=? x
        b @=? y
        c @=? z
    ]
