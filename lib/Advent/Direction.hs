module Advent.Direction where

data Direction = DUp | DDown | DLeft | DRight
  deriving (Ord, Eq)

instance Show Direction where
  show DUp = "U"
  show DDown = "D"
  show DLeft = "L"
  show DRight = "R"

inverse :: Direction -> Direction
inverse DUp = DDown
inverse DDown = DUp
inverse DLeft = DRight
inverse DRight = DLeft

directions :: [Direction]
directions = [DUp, DDown, DLeft, DRight]
