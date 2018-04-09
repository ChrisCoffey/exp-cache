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
        ],
    testCase "empty strategy eviction is a no-op" $ do
        let rr = newRR defaultGen 10 :: RR Int
            (rr', res) = evict rr
        res @=? Nothing
        rr' @=? rr
    ]


defaultGen :: StdGen
defaultGen = mkStdGen 25

insertN ::
    RR Int
    -> Int
    -> RR Int
insertN rr n = foldr recordLookup rr [0..n]
