module Advent.Direction where

data Direction = DUp | DDown | DLeft | DRight

instance Show Direction where
  show DUp = "U"
  show DDown = "D"
  show DLeft = "L"
  show DRight = "R"

inverse DUp = DDown
inverse DDown = DUp
inverse DLeft = DRight
inverse DRight = DLeft
