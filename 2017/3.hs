{-# LANGUAGE NamedFieldPuns #-}
import Advent
import qualified Advent.Coord as Coord
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Control.Monad.State
import Control.Monad.Writer

spiral :: [Direction]
spiral = [DRight, DUp, DLeft, DDown]

origin :: Coord
origin = Coord 0 0

allCoords :: [Coord]
allCoords = execWriter (allCoords' origin spiral 1)
  where
    allCoords' :: Coord -> [Direction] -> Int -> Writer [Coord] ()
    allCoords' start (stepA:stepB:dirs) n = do
      start <- foldM (f stepA) start [0..n - 1]
      start <- foldM (f stepB) start [0..n - 1]
      allCoords' start (dirs ++ [stepA, stepB]) (n + 1)

    f :: Direction -> Coord -> Int -> Writer [Coord] Coord
    f dir c _ = do
      tell [c]
      return (move dir c)

allSums :: [Int]
allSums = execWriter (execStateT (allSums' 1) (Map.singleton origin 1))
  where
    allSums' :: Int -> StateT (Map Coord Int) (Writer [Int]) ()
    allSums' n = do
      m <- get
      let coord = allCoords !! n
      let ns = neighbors8 coord
      let v = sum . mapMaybe (`Map.lookup` m) $ ns
      put (Map.insert coord v m)
      lift (tell [v])
      allSums' (n + 1)

findCoord :: Int -> Coord
findCoord n = allCoords !! (n - 1)

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
      solve2 = head . ($ allSums) . dropWhile . (>) . read
