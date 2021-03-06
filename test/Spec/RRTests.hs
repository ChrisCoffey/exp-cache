module Spec.RRTests (
    rrSpec
) where

import Data.Cache
import Data.Cache.Eviction.RR

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import System.Random (StdGen, mkStdGen)

rrSpec :: TestTree
rrSpec = testGroup "RR Spec" [
    testGroup "size never exceeds upper bound" [
        testCase "size = 1" $ do
            let rr = newRR defaultGen 1
                rr' = insertN rr 2
            1  @=? rrSizeDebug rr'
        , testCase "size = 1000" $ do
            let rr = newRR defaultGen 1000
                rr' = insertN rr 5000
            1000 @=? rrSizeDebug rr'
        ]
    , testCase "empty strategy eviction is a no-op" $ do
        let rr = newRR defaultGen 10 :: RR Int
            (rr', res) = evict rr
        res @=? Nothing
        rr' @=? rr
    , testCase "evict returns last evicted" $ do
        let rr = newRR defaultGen 10 :: RR Int
            rr' = insertN rr 1000
            (_, evicted) = evict rr'
        Just 5 @=? evicted
    , testCase "evict is deterministic given a seed" $ do
        let rr = newRR defaultGen 10 :: RR Int
            rr' = insertN rr 1000
            (e1, evicted) = evict rr'
            e2 = recordLookup 2000 e1
            (e3, evicted') = evict e2
            e4 = recordLookup 10000 e3
            (_, evicted'') = evict e4
        Just 5 @=? evicted
        Just 7 @=? evicted'
        Just 10 @=? evicted''
    ]


defaultGen :: StdGen
defaultGen = mkStdGen 25

insertN ::
    RR Int
    -> Int
    -> RR Int
insertN rr n = foldr recordLookup rr [0..n]
