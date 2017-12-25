import Advent

import Data.Bifunctor

data HexDirection = NW | N | NE | SE | S | SW deriving (Show, Read)

-- N/S, NW/SE, [NE/SW]
type Position = (Int, Int)

walk :: Position -> HexDirection -> Position
walk (x, y) dir =
  case dir of
    N -> (x - 1, y)
    S -> (x + 1, y)
    NW -> (x, y - 1)
    SE -> (x, y + 1)
    NE -> (x - 1, y + 1)
    SW -> (x + 1, y - 1)

uppercase :: String -> String
uppercase = map toUpper

hexDistance :: Position -> Int
hexDistance (x, y) = maximum . map abs $ [x, y, z]
  where z =  -x - y

main :: IO ()
main = solve parse part1 part2
  where
    parse = map (read . uppercase) . splitOn ","
    part1 = hexDistance . foldl walk (0, 0)
    part2 = maximum . map hexDistance . scanl walk (0,0)
