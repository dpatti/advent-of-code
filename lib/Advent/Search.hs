module Advent.Search where

import Control.Monad.State.Lazy
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set

data Memo a b = Memo { memoValue :: a
                     , memoKey :: b
                     }

instance (Eq b) => Eq (Memo a b) where
  (==) = (==) `on` memoKey

instance (Ord b) => Ord (Memo a b) where
  compare = compare `on` memoKey

-- Does a bfs using a neighbor lookup function
bfs :: (a -> [a]) -> [a] -> [a]
bfs _ [] = []
bfs getNeighbors nodes = nodes ++ traverse next
  where
    traverse = bfs getNeighbors
    next = concatMap getNeighbors nodes

-- Does a bfs using a neighbor lookup function under a monad. Careful! The bind
-- here means that you might not see results until the entire search is
-- exhausted unless you're using a monad that works lazily under bind. For
-- example, `Control.Monad.State.Lazy` is likely preferable to its Strict
-- counterparty.
bfsM :: Monad m => (a -> m [a]) -> [a] -> m [a]
bfsM _ [] = return []
bfsM getNeighbors nodes = do
  rest <- next >>= bfsM getNeighbors
  return $ nodes ++ rest
  where
    next = concat <$> mapM getNeighbors nodes

memoM :: (Monad m, Ord b) => (a -> b) -> (a -> m [a]) -> a -> StateT (Set b) m [a]
memoM f search start = do
  modify $ Set.insert (f start)
  lift (search start) >>= filterM (\neighbor -> do
    let key = f neighbor
    wasVisited <- gets $ Set.member key
    unless wasVisited (modify $ Set.insert key)
    return (not wasVisited))

memo :: Ord b => (a -> b) -> (a -> [a]) -> a -> State (Set b) [a]
memo f = memoM f . fmap return

memoId :: Ord a => (a -> [a]) -> a -> State (Set a) [a]
memoId = memo id

runMemo :: State (Set b) [a] -> [a]
runMemo s = evalState s Set.empty

-- These below implementations came before bfsM and still could be useful if the
-- lazy state in memo performs poorly

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
