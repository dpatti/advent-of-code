module Advent.Search where

import Advent.Function
import Data.Set (Set)
import qualified Data.Set as Set

data Memo a b = Memo { memoValue :: a
                     , memoKey :: b
                     }

instance (Eq b) => Eq (Memo a b) where
  (==) = (==) `with` memoKey

instance (Ord b) => Ord (Memo a b) where
  compare = compare `with` memoKey

-- Does a bfs using a neighbor lookup function
bfs :: (a -> [a]) -> [a] -> [a]
bfs _ [] = []
bfs getNeighbors nodes = nodes ++ traverse next
  where
    traverse = bfs getNeighbors
    next = concatMap getNeighbors nodes

-- Does a bfs storing visited states in a set so that they won't be visited
-- again
bfsMemo :: (Ord a) => (a -> [a]) -> [a] -> [a]
bfsMemo = bfs' Set.empty
  where
    bfs' :: (Ord a) => Set a -> (a -> [a]) -> [a] -> [a]
    bfs' _ _ [] = []
    bfs' s getNeighbors nodes = nodes ++ traverse unique
      where
        debug = "n=" ++ show (length nodes) ++ "; s=" ++ show (Set.size s) ++ "; u=" ++ show (length unique) ++ "; s'=" ++ show (Set.size s)
        traverse = bfs' s' getNeighbors
        (s', unique) = foldr digest (s, []) next
        next = concatMap getNeighbors nodes

        digest :: Ord a => a -> (Set a, [a]) -> (Set a, [a])
        digest x (s, xs)
          | Set.member x s = (s, xs)
          | otherwise = (Set.insert x s, x:xs)

bfsMemoBy :: Ord b => (a -> b) -> (a -> [a]) -> [a] -> [a]
bfsMemoBy f search = map fromMemo . bfsMemo search' . map toMemo
  where
    search' = map toMemo . search . fromMemo
    toMemo value = Memo value (f value)
    fromMemo = memoValue
