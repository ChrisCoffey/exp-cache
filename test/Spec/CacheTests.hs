module Spec.CacheTests (
    cacheTests
) where

import Data.Cache

import Control.Exception
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Test.HUnit                   ((@=?))
import Test.Tasty                   (TestTree, testGroup)
import Test.Tasty.HUnit             (testCase)

type DefaultCache = Cache Int Int (FIFO Int)

cacheTests :: TestTree
cacheTests = testGroup "Cache" [
    testCase "Cannot initialize a 0 sized cache" $ do
        res <- catch ((pure $! (newCache 0 newFIFO :: DefaultCache)) *> pure False) (\(e::SomeException) -> pure True)
        res @=? True
    ,testCase "Verify that a cache size of 1 stores an element before eviction" $ do
        r <- newIORef 0
        let cache = newCache 1 newFIFO
        (v,cache') <- readThrough cache 10 (const $ pure 1 <* writeIORef r 1) :: IO (Int, DefaultCache)
        _ <- readThrough cache' 10 (const $ pure 1 <* writeIORef r 2)
        calledLookup <- readIORef r
        calledLookup @=? 1
    ,testCase "Verify that the proper eviction strategy is evaluated (FIFO vs. LRU)" $ do
        let cache = newCache 1 newFIFO
        r <- newIORef False
        (_,cache') <- readThrough cache 10 (const $ pure 1) :: IO (Int, DefaultCache)
        (_,cache'') <- readThrough cache' 11 (const $ pure 1)
        _ <- readThrough cache'' 10 (const $ pure 1 <* writeIORef r True)
        v <- readIORef r
        v @=? True
    ,testCase "Verify that eviction only fires once the cache is full" $ do
        let cache = newCache 1 newFIFO
        r <- newIORef False
        (_,cache') <- readThrough cache 10 (const $ pure 1) :: IO (Int, DefaultCache)
        (_,cache'') <- readThrough cache' 11 (const $ pure 1)
        _ <- readThrough cache'' 10 (const $ pure 1 <* writeIORef r True)
        v <- readIORef r
        v @=? True
    ,testCase "Verify that the read function evaluates only if the key is not in the cache" $ do
        let cache = newCache 100 newFIFO
        r <- newIORef True
        (_,cache') <- readThrough cache 10 (const $ pure 1) :: IO (Int, DefaultCache)
        _ <- readThrough cache' 10 (const $ pure 1 <* writeIORef r False)
        v <- readIORef r
        v @=? True
    ,testCase "Verify that the value returned by the read function is stored in the cache" $ do
        let cache = newCache 100 newFIFO
        r <- newIORef True
        (v,cache') <- readThrough cache 10 (const $ pure 1) :: IO (Int, DefaultCache)
        v @=? 1
    ]

--TODO make a note that this is not the most memory efficient, but it is time efficient

