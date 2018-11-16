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

cwTurn :: Direction -> Direction
cwTurn DUp = DRight
cwTurn DDown = DLeft
cwTurn DLeft = DUp
cwTurn DRight = DDown

ccwTurn :: Direction -> Direction
ccwTurn DUp = DLeft
ccwTurn DDown = DRight
ccwTurn DLeft = DDown
ccwTurn DRight = DUp

directions :: [Direction]
directions = [DUp, DDown, DLeft, DRight]
