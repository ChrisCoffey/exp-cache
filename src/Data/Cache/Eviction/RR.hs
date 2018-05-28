module Data.Cache.Eviction.RR (
    RR,
    newRR,
    rrSizeDebug
) where

import Data.Cache.Eviction

import qualified Data.HashSet as S
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import System.Random (StdGen, randomR)

-- | Random Replacement cache. The seed is fixed to an 'StdGen' since its both
-- easily accessible & good enough for this purpose. Random replacement means exactly what it
-- sounds like: when the cache fills up a random element is selected and evicted.
data RR k = RR {
    seed :: !StdGen,
    writeCell :: !Int,
    upperBound :: !Int,
    overwritten :: Maybe k, -- This is a wart used for tracking evicted nodes
    contents :: StupidBiMap k
    } deriving (Show)

instance Eq k => Eq (RR k) where
    (==) l r = writeCell l == writeCell r &&
             upperBound l == upperBound r &&
             contents l == contents r

-- | Generate a new Random Replacement cache using the provided seed & size.
newRR ::
    StdGen
    -> Int
    -> RR k
newRR gen upperBound = RR gen 0 upperBound Nothing (StupidBiMap M.empty M.empty)

instance EvictionStrategy RR where
    recordLookup key rr@(RR {seed, upperBound , writeCell, contents=c@(StupidBiMap idxM kM) })
        -- When the key has already been stored, take no action
        | knownKey key c = rr
        -- When its a new key & the cache is full,
        | M.size idxM == upperBound = let
            (nextCell, seed') = randomR (0, upperBound -1) seed
            valAtIndex = keyAtIndex writeCell c
            in RR {
                writeCell = nextCell,
                seed = seed',
                upperBound = upperBound,
                overwritten = valAtIndex,
                contents = recordPair key writeCell c
                }
        | M.size idxM < upperBound =
            rr {writeCell = min (writeCell + 1) (upperBound -1), contents = recordPair key writeCell c}
        | otherwise = rr

    evict rr@(RR {overwritten} ) = (rr {overwritten=Nothing} , overwritten)

-- Horrible space efficiency
data StupidBiMap k = StupidBiMap (M.Map Int k) (M.Map k Int)
    deriving (Eq, Show)

recordPair :: (Eq k, Ord k) =>
    k
    -> Int
    -> StupidBiMap k
    -> StupidBiMap k
recordPair k writeIndex (StupidBiMap idxM kM) =
    StupidBiMap (M.insert writeIndex k idxM) (M.insert k writeIndex kM)

keyAtIndex ::
    Int
    -> StupidBiMap k
    -> Maybe k
keyAtIndex idx (StupidBiMap idxM _) =
    M.lookup idx idxM

knownKey :: (Eq k, Ord k) =>
    k
    -> StupidBiMap k
    -> Bool
knownKey k (StupidBiMap _ kM) =
    isJust $ M.lookup k kM

rrSizeDebug ::
    RR k
    -> Int
rrSizeDebug RR {contents = StupidBiMap l _} = M.size l
