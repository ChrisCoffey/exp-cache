module Benchmarks.LFU (
    lfuBenchmarks
) where

import Data.Cache

import Criterion
import Control.Monad (void)
import Data.Foldable (foldl')
import System.Random (newStdGen, randomR)

lfuBenchmarks :: Benchmark
lfuBenchmarks = bgroup "lfu" [
    bench "recordLookup 1000 distinct" $ whnf storeNDistinct 1000,
    bench "recordLookup 10K distinct" $ whnf storeNDistinct 10000,
    bench "store And Evict 1000" $ whnfIO (storeAndEvict 1000),
    bench "store And Evict 10k" $ whnfIO (storeAndEvict 100000),
    bench "store And Evict 100k" $ whnfIO (storeAndEvict 1000000)
    ]
    where
        s :: LFU Int
        s = newLFU
        storeNDistinct n = foldr recordLookup s [0..n]

        storeAndEvict :: Int -> IO (LFU Int)
        storeAndEvict n = do
            gen <- newStdGen
            xs <- pure $ storeNDistinct (n `div` 10)
            lookups <- pure . fst $ foldl' step ([], gen) [0..n]
            res <- pure $ foldl' runLFU xs lookups
            pure res
            where
                step (acc, gen) _ = let
                    (x, gen') = randomR (0,n `div` 10) gen
                    in (x:acc, gen')
                runLFU lfu x = fst . evict $ recordLookup x lfu
