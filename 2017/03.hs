{-# LANGUAGE NamedFieldPuns #-}
import Advent
import qualified Advent.Coord as Coord
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Control.Monad.State
import Control.Monad.Writer

spiralDirections :: [Direction]
spiralDirections = cycle [DRight, DUp, DLeft, DDown]

origin :: Coord
origin = Coord 0 0

timesM :: (Monad m) => Int -> (a -> m a) -> a -> m a
timesM n f x = foldM ((f .) <$> const) x [1 .. n]

-- First implementation with a writer monad
spiralCoords' :: [Coord]
spiralCoords' = DList.toList $ execWriter (loop origin spiralDirections 1)
  where
    loop :: Coord -> [Direction] -> Int -> Writer (DList Coord) ()
    loop start dirs steps = do
      end <- foldM (walk steps) start (take 2 dirs)
      loop end (drop 2 dirs) (steps + 1)

    walk :: Int -> Coord -> Direction -> Writer (DList Coord) Coord
    walk steps start dir =
      timesM steps (step dir) start

    step :: Direction -> Coord -> Writer (DList Coord) Coord
    step dir coord =
      tell (return coord) $> move dir coord

-- Without writer monad, and much simpler
spiralCoords :: [Coord]
spiralCoords =
  scanl (flip move) origin . concat . concat $
    zipWith (map . replicate) [1..] (chunks 2 spiralDirections)

spiralSums :: [Int]
spiralSums = evalState computation initialGrid
  where
    initialGrid :: Map Coord Int
    initialGrid = Map.singleton origin 1

    computation :: State (Map Coord Int) [Int]
    computation =
      forM (drop 1 spiralCoords) $ \coord -> do
        grid <- get
        let value = sum . mapMaybe (`Map.lookup` grid) . neighbors8 $ coord
        modify (Map.insert coord value)
        return value

findCoord :: Int -> Coord
findCoord n = spiralCoords !! (n - 1)

distanceTo :: Coord -> Coord -> Int
distanceTo c1 c2 =
  abs (Coord.x c1 - Coord.x c2) + abs (Coord.y c1 - Coord.y c2)

main :: IO ()
main = do
  input <- getContents
  print . solve1 $ input
  print . solve2 $ input
    where
      solve1 = distanceTo origin . findCoord . read
      solve2 = head . ($ spiralSums) . dropWhile . (>) . read
