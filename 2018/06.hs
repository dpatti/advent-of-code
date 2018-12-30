{-# LANGUAGE NamedFieldPuns #-}

import Advent

import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map

coordParser :: Parser Coord
coordParser = Coord <$> decimal <* symbol "," <*> decimal

type Grid a = Map Coord a
type Landmark = Int
type Distance = Int

landmarks :: [Coord] -> Grid Landmark
landmarks = Map.fromList . flip zip [0..]

bounds :: Grid a -> (Coord, Coord)
bounds = Map.foldlWithKey tighten (topLeft, bottomRight)
  where
    topLeft = Coord maxBound maxBound
    bottomRight = Coord minBound minBound
    tighten (Coord xMin yMin, Coord xMax yMax) (Coord x y) _ =
      (Coord (min x xMin) (min y yMin), Coord (max x xMax) (max y yMax))

data Tag = Tag { distance :: Int, landmark :: Int }
  deriving Show

fillBounds :: Grid Landmark -> Grid Landmark
fillBounds grid =
  fmap landmark . updateWith Map.empty . bfsM step . toTagList $ grid
 where
   toTagList = map (fmap (Tag 0)) . Map.toList
   (Coord leftBound topBound, Coord rightBound bottomBound) = bounds grid

   outOfBounds :: Coord -> Bool
   outOfBounds (Coord x y)
     | x < leftBound = True
     | x > rightBound = True
     | y < topBound = True
     | y > bottomBound = True
     | otherwise = False

   step :: (Coord, Tag) -> State (Map Coord Tag) [(Coord, Tag)]
   step (pos, tag) = do
     loc <- gets $ Map.lookup pos
     -- size <- gets Map.size
     -- when (landmark tag == 3) $
     --   traceShowM (size, pos, tag, loc)
     case (outOfBounds pos, loc) of
       (True, _) -> return []
       (False, Nothing) -> do
         modify $ Map.insert pos tag
         let tag' = tag { distance = distance tag + 1 }
         return $ fmap (flip (,) tag') (neighbors pos)
       (False, Just existing) -> do
         when (distance tag == distance existing && landmark tag /= landmark existing) $
           modify $ Map.insert pos tag{landmark = -1}
         return []

removeUnbounded :: Grid Landmark -> Grid Landmark
removeUnbounded grid = foldl' f Map.empty . Map.toList $ grid
  where
    (Coord leftBound topBound, Coord rightBound bottomBound) = bounds grid

    unbounded :: [Int]
    unbounded = Map.elems . Map.filterWithKey (const . onBoundary)  $ grid

    onBoundary :: Coord -> Bool
    onBoundary (Coord x y) =
      x `elem` [leftBound, rightBound] || y `elem` [topBound, bottomBound]

    f grid (coord, landmark) =
      if landmark `elem` unbounded
         then grid
         else Map.insert coord landmark grid

largestArea :: Grid Landmark -> Int
largestArea = maximum . freqs . Map.elems

withinDistance :: Int -> Grid Landmark -> [Coord]
withinDistance maxDistance grid = do
  x <- [leftBound .. rightBound]
  y <- [topBound .. bottomBound]
  let coord = Coord x y
  guard $ distances coord < maxDistance
  return coord

 where
  (Coord leftBound topBound, Coord rightBound bottomBound) = bounds grid
  landmarkLocs = Map.keys grid
  distances coord = getSum . foldMap (Sum . manhattanDistance coord) $ landmarkLocs

main :: IO ()
main = solve parse part1 part2
  where
    parse = map (parseWith coordParser) . lines
    part1 = largestArea . removeUnbounded . fillBounds . landmarks
    part2 = length . withinDistance 10000 . landmarks
