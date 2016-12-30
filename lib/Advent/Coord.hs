module Advent.Coord where

import Advent.Bounded
import Advent.Direction
import Data.Monoid

data Coord = Coord { x :: Int, y :: Int } deriving (Eq, Ord)

instance Show Coord where
  show (Coord x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Monoid Coord where
  mempty = Coord 0 0
  mappend (Coord x1 y1) (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)

-- Coord map
cmap :: (Int -> Int) -> Coord -> Coord
cmap f (Coord x y) = Coord (f x) (f y)

move :: Direction -> Coord -> Coord
move DUp = (Coord 0 (-1) <>)
move DDown = (Coord 0 1 <>)
move DLeft = (Coord (-1) 0 <>)
move DRight = (Coord 1 0 <>)

neighbors :: Coord -> [Coord]
neighbors pos = move <$> [DUp, DDown, DLeft, DRight] <*> [pos]

boundCoord :: Coord -> Coord -> Coord -> Coord
boundCoord min max target = Coord (boundWith x) (boundWith y)
  where
    boundWith = bound <$> ($ min) <*> ($ max) <*> ($ target)

coordInBounds :: Coord -> Coord -> Coord -> Bool
coordInBounds lower upper target = target == boundCoord lower upper target