module Benchmarks.LRU (
    lruBenchmarks
    ) where

import Data.Cache

import Criterion
import Control.Monad (void)
import Data.Foldable (foldl')
import System.Random (newStdGen, randomR)

lruBenchmarks :: Benchmark
lruBenchmarks = bgroup "lru" [
    lruSeqBenchmarks,
    lruHashBenchmarks
    ]

lruSeqBenchmarks :: Benchmark
lruSeqBenchmarks = bgroup "sequential" [
    bench "recordLookup 1000 distinct" $ whnf storeNDistinct 1000,
    bench "recordLookup 10K distinct" $ whnf storeNDistinct 10000,
    bench "store And Evict 1000" $ whnfIO (storeAndEvict 1000),
    bench "store And Evict 10k" $ whnfIO (storeAndEvict 10000)
    ]
    where
        s :: SeqLRU Int
        s = newSeqLRU
        storeNDistinct n = foldr recordLookup s [0..n]

        storeAndEvict :: Int -> IO (SeqLRU Int)
        storeAndEvict n = do
            gen <- newStdGen
            xs <- pure $ storeNDistinct n
            lookups <- pure . fst $ foldl' step ([], gen) [0..n]
            res <- pure $ foldl' runLRU xs lookups :: IO (SeqLRU Int)
            pure res
            where
                step (acc, gen) _ = let
                    (x, gen') = randomR (0,n) gen
                    in (x:acc, gen')
                runLRU lru x = fst . evict $ recordLookup x lru

lruHashBenchmarks :: Benchmark
lruHashBenchmarks = bgroup "hash priority search queue" [
    bench "recordLookup 1000 distinct" $ whnf storeNDistinct 1000,
    bench "recordLookup 10K distinct" $ whnf storeNDistinct 10000,
    bench "store And Evict 1000" $ whnfIO (storeAndEvict 1000),
    bench "store And Evict 10k" $ whnfIO (storeAndEvict 10000),
    bench "store And Evict 100k" $ whnfIO (storeAndEvict 100000)
    ]
    where
        s :: LRU Int
        s = newLRU
        storeNDistinct n = foldr recordLookup s [0..n]

        storeAndEvict :: Int -> IO (LRU Int)
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
                runLRU lru x = fst . evict $ recordLookup x lru
