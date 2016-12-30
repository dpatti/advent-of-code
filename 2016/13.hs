-- {-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad.State
import Data.Bifunctor
import Data.Bits
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import Debug.Trace

data Cell = Wall | Open deriving Eq
type Coord = (Int, Int)
type Paths = Map Coord Int

neighbors :: (Coord -> Cell) -> Coord -> [Coord]
neighbors plot coord = filter ((&&) <$> isValid <*> isOpen . plot) adjacent
  where
    isValid (x, y) = x >= 0 && y >= 0
    isOpen = (Open ==)
    adjacent = map (bimap (fst coord +) (snd coord +)) [(1,0), (-1,0), (0,1), (0,-1)]

upsert :: Ord k => (Maybe a -> a) -> k -> Map k a -> Map k a
upsert f = Map.alter (Just . f)

distances :: (Coord -> Cell) -> Coord -> [(Coord, Int)]
distances plot start = flip evalState Map.empty . distances' $ [(start, 0)]
  where
    distances' :: [(Coord, Int)] -> State Paths [(Coord, Int)]
    distances' nodes = do
      nodes' <- flip filterM nodes $ \(coord, dist) -> do
        -- return True (why was this here?)
        record <- gets (Map.lookup coord)
        case record of
          (Just dist) -> return False
          Nothing -> modify (Map.insert coord dist) >> return True

      (nodes' ++) <$> distances' (concatMap visit nodes')

    visit :: (Coord, Int) -> [(Coord, Int)]
    visit (c, dist) = map (, dist + 1) (neighbors plot c)

    -- I don't think this works because something about lazy lists inside of a
    -- monad?
    -- distances coords = do
    --   dists <- forM coords $ \coord -> do
    --     dist <- gets (Map.! coord)
    --     traceShowM (coord, dist)
    --     let ns = neighbors plot coord
    --     let nsDist = dist + 1
    --     -- update neighbors
    --     forM_ ns (modify . upsert (maybe nsDist (min nsDist)))
    --     return dist

    --   let these = zip coords dists

    --   rest <- distances (concatMap (neighbors plot) coords)

    --   return $ these ++ rest

formula :: Int -> Coord -> Cell
formula fav (x, y)
  | even bits = Open
  | odd bits = Wall
  where
    bits = popCount n
    n = x*x + 3*x + 2*x*y + y + y*y + fav

main = do
  input <- parse <$> getContents
  -- print . solve1 $ input
  print . solve2 $ input

 where
   parse = read
   -- solve1 = snd . fromJust . find ((== (31, 39)) . fst) . nodes
   solve2 = length . takeWhile ((<= 50) . snd) . nodes

   nodes i = distances (formula i) (1, 1)
