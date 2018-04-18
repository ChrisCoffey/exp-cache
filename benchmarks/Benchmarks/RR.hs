module Benchmarks.RR (
    rrBenchmarks
    ) where

import Data.Cache

import Criterion
import Control.Monad (void)
import Data.Foldable (foldl')
import System.Random (newStdGen, randomR, StdGen)
import System.IO.Unsafe (unsafePerformIO)

defaultGen :: StdGen
defaultGen = unsafePerformIO newStdGen
{-# NOINLINE defaultGen #-}

rrBenchmarks :: Benchmark
rrBenchmarks = bgroup "rr" [
    bench "recordLookup 1000 distinct" $ whnf storeNDistinct 1000,
    bench "recordLookup 10K distinct" $ whnf storeNDistinct 10000,
    bench "store And Evict 1000" $ whnfIO (storeAndEvict 1000),
    bench "store And Evict 10k" $ whnfIO (storeAndEvict 10000),
    bench "store And Evict 100k" $ whnfIO (storeAndEvict 100000)
    ]
    where
        s :: StdGen -> Int -> RR Int
        s = newRR
        storeNDistinct n =
            foldr recordLookup (s defaultGen n) [0..n]

        storeAndEvict :: Int -> IO (RR Int)
        storeAndEvict n = do
            gen <- newStdGen
            xs <- pure $ storeNDistinct n
            lookups <- pure . fst $ foldl' step ([], gen) [0..n]
            res <- pure $ foldl' runLRU xs lookups
            pure res
            where
                step (acc, gen) _ = let
                    (x, gen') = randomR (0,n) gen
                    in (x:acc, gen')
                runLRU rr x = fst . evict $ recordLookup x rr
