{-# LANGUAGE NamedFieldPuns, TupleSections #-}

import Advent hiding (distance)

import Control.Monad.State
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Debug.Trace

data Tile = Wall | Floor | Spot Int deriving (Eq, Show)
data Graph = Graph { locations :: Map Int Coord
                   , layout :: Map Coord Tile
                   , distance :: Coord -> Coord -> Int
                   }

instance Show Graph where
  show = show . ((,) <$> locations <*> layout)

parseTile :: Char -> Tile
parseTile '#' = Wall
parseTile '.' = Floor
parseTile d = Spot (read [d])

items = 7

findShortestPath Graph { locations, layout, distance } =
  minimum . map (routeCost . (++ [toCoord 0]) . (toCoord 0 :)) . permutations . map toCoord $ [1..items]

  where
    toCoord = (locations Map.!)
    routeCost = sum . map (uncurry distance . tup2) . eachCons 2
    tup2 [a, b] = (a, b)

buildGraph :: [[Tile]] -> Graph
buildGraph xss = Graph { locations, layout, distance }
  where
    locations = updateWith Map.empty . sequence $ do
      (row, y) <- zip xss [0..]
      (tile, x) <- zip row [0..]

      case tile of
        (Spot n) -> return $ modify (Map.insert n (Coord x y))
        _ -> []

    layout = updateWith Map.empty . sequence $ do
      (row, y) <- zip xss [0..]
      (tile, x) <- zip row [0..]

      return $ modify (Map.insert (Coord x y) tile)

    -- distance from to = fromDistance $ distances Map.! (from, to)
    distance from to = distances' Map.! (from, to)

    locs = Map.elems locations

    distances' :: Map (Coord, Coord) Int
    distances' = updateWith Map.empty $
      forM (combinations 2 locs) $ \[from, to] -> do
        let distance = path from to

        modify (Map.insert (from, to) distance)
        modify (Map.insert (to, from) distance)

    path :: Coord -> Coord -> Int
    path from to = snd . fromJust . find ((== to) . fst) . bfsMemoBy fst step . traceShowId $ [(from, 0)]
      where
        step :: (Coord, Int) -> [(Coord, Int)]
        step (coord, s) = map (, s+1) . filter ((/= Wall) . (layout Map.!)) $ neighbors coord

    {-
    coords = Map.keys layout

    distances :: Map (Coord, Coord) Distance
    distances = updateWith init . sequence $ do
      k <- coords
      i <- coords
      j <- coords

      return $ do
        ij <- gets (Map.! (i, j))
        ik <- gets (Map.! (i, k))
        kj <- gets (Map.! (k, j))

        let path' = ik <> kj
        when (ij > path') $
          modify (Map.insert (i, j) path')

    init :: Map (Coord, Coord) Distance
    init = foldl' setup Map.empty (allPairs coords)

    setup map pair@(from, to)
      | from == to = Map.insert pair (Distance 0) map
      | adjacent from to && Wall `notElem` ((layout Map.!) <$> [from, to]) = Map.insert pair (Distance 1) map
      | otherwise = Map.insert pair Undetermined map
    -}

adjacent :: Coord -> Coord -> Bool
adjacent a b = b `elem` (move <$> [DUp, DDown, DLeft, DRight] <*> [a])

main = do
  input <- parse <$> getContents
  print . solve1 $ input
  
 where
   parse = map (map parseTile) . lines
   solve1 = findShortestPath . buildGraph
