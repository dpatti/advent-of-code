import Data.List
import qualified Data.Matrix as Matrix
import Data.Maybe
import Debug.Trace

data Status = On | Off deriving (Show, Eq)
type Pos = (Int, Int)
type Lights = Matrix.Matrix Status

fromChar :: Char -> Status
fromChar '.' = Off
fromChar '#' = On

-- annotate :: [a] -> [(a, Int)] 
-- annotate xs = zip xs nats
--   where nats = 0 : map (+1) nats
-- 
-- foldMatrix :: (b -> (a, Int, Int) -> b) -> b -> [[a]] -> b
-- foldMatrix f init = foldl' f init . expand
--   where
--     expand :: [[a]] -> [(a, Int, Int)]
--     expand rows = do
--       (cols, i) <- annotate rows
--       (cell, j) <- annotate cols
-- 
--       return (cell, i, j)
-- 
-- mapMatrix :: ((a, Int, Int) -> b) -> [[a]] -> [[b]]
-- mapMatrix = 

parseConfig :: [[Char]] -> Lights
parseConfig = fmap fromChar . Matrix.fromLists
--  where
--    build lights (c, i, j) = Map.insert (i, j) (fromChar c) lights

step :: Lights -> Lights
step lights = Matrix.matrix rows cols generation
  where
    rows = Matrix.nrows lights
    cols = Matrix.ncols lights

    generation :: (Int, Int) -> Status
    generation (i, j) = toggle (get (i, j)) (neighbors (i, j))

    toggle :: Status -> Int -> Status
    toggle On n | n `elem` [2, 3] = On
    toggle Off n | n == 3 = On
    toggle _ _ = Off

    neighbors :: Pos -> Int
    neighbors = length . filter (== On) . Matrix.toList . boundingBox

    boundingBox :: Pos -> Lights
    boundingBox = Matrix.setElem Off (2, 2) . box
    box (i, j) = submatrix (i - 1) (j - 1) (i + 1) (j + 1)
    -- Have to make our own for out of bounds
    submatrix :: Int -> Int -> Int -> Int -> Lights
    submatrix x1 y1 x2 y2 = Matrix.fromLists $ do
      i <- [x1..x2]
      return $ do
        j <- [y1..y2]
        return $ get (i, j)
    -- Force corner lights too
    get :: (Int, Int) -> Status
    get (i, j)
      | (i, j) == (1, 1) = On
      | (i, j) == (rows, 1) = On
      | (i, j) == (1, cols) = On
      | (i, j) == (rows, cols) = On
      | otherwise = fromMaybe Off . Matrix.safeGet i j $ lights

brokenStep :: Lights -> Lights
brokenStep = fix . step . fix
  where fix m = Matrix.setElem On (1, 1)
              $ Matrix.setElem On (Matrix.nrows m, 1)
              $ Matrix.setElem On (1, Matrix.ncols m)
              $ Matrix.setElem On (Matrix.nrows m, Matrix.ncols m)
              m

-- step lights = mapMatrix (step' lights) lights
--   where
--     step' lights (c, i, j) = Map.insert (i, j) lights $ neighbors (i, j) lights
-- 
--     neighbors (i, j) = undefined

main :: IO ()
main = do
  lights <- parseConfig . lines <$> getContents
  -- print . (!! 0) . run $ lights
  -- print . (!! 1) . run $ lights
  -- print . (!! 2) . run $ lights
  -- print . (!! 5) . run $ lights
  print . length . filter (== On) . Matrix.toList . (!! 100) . run $ lights
  where run = iterate brokenStep
