{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.State
import Control.Applicative
import Data.Attoparsec.Text hiding (take)
import Debug.Trace
import qualified Data.Matrix as Matrix
import qualified Data.Vector as Vector
import qualified Data.Text as T

data Command = Rect { w :: Int, h :: Int }
             | RotateRow { y :: Int, by :: Int }
             | RotateCol { x :: Int, by :: Int }
             deriving Show

data Pixel = On | Off deriving Eq

instance Show Pixel where
  show On = "#"
  show Off = "."

type Screen = Matrix.Matrix Pixel

fromRight (Left e) = error e
fromRight (Right v) = v

parseCommand :: T.Text -> Command
parseCommand = fromRight . parseOnly
  (rectParser <|> rotateRowParser <|> rotateColParser)

  where
    rectParser = do
      string "rect "
      w <- decimal
      char 'x'
      h <- decimal

      return Rect { w = w, h = h }

    rotateRowParser = do
      string "rotate row y="
      y <- decimal
      string " by "
      by <- decimal

      return RotateRow { y = y, by = by}

    rotateColParser = do
      string "rotate column x="
      x <- decimal
      string " by "
      by <- decimal

      return RotateCol { x = x, by = by}

countOn :: Screen -> Int
countOn (Matrix.toList -> xs) = length (filter (== On) xs)

updateWith :: a -> [State a ()] -> a
updateWith s = (`execState` s) . sequence

set :: Pixel -> (Int, Int) -> Screen -> Screen
set s (x, y) = Matrix.setElem s (x + 1, y + 1)

setSeries :: (Int -> (Int, Int)) -> Vector.Vector Pixel -> Screen -> Screen
setSeries coord (Vector.toList -> series) screen = updateWith screen $ do
  (value, i) <- zip series [0..]
  return . modify $ set value (coord i)

applyCommand :: Screen -> Command -> Screen
applyCommand screen (Rect w h) = updateWith screen $ do
  x <- [0..h - 1]
  y <- [0..w - 1]

  return . modify $ set On (x, y)

applyCommand screen (RotateRow x by) = updateWith screen . return $ do
  row' <- gets $ shift by . Matrix.getRow (x + 1)
  modify $ setSeries (\y -> (x,y)) row'

applyCommand screen (RotateCol y by) = updateWith screen . return $ do
  col' <- gets $ shift by . Matrix.getCol (y + 1)
  modify $ setSeries (\x -> (x,y)) col'

shift :: Show a => Int -> Vector.Vector a -> Vector.Vector a
shift n vec = Vector.drop offset vec Vector.++ Vector.take offset vec
  where offset = Vector.length vec - n

-- rows = 3
-- cols = 7
rows = 6
cols = 50
screen = Matrix.matrix rows cols (const Off)

main = do
  input <- parse <$> getContents
  -- print input
  print . solve1 $ input
  print . solve2 $ input

  where
    parse = map (parseCommand . T.pack) . lines
    solve1 = countOn . foldl applyCommand screen
    solve2 = foldl applyCommand screen
