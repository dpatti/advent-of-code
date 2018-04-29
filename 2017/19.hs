import Advent

import Control.Monad.Writer
import Data.Vector (Vector)
import qualified Data.Vector as Vector

type Network = Vector (Vector Char)

type State = (Direction, Coord)

start :: Network -> State
start = (,) DDown . flip Coord 0 . fromJust . Vector.elemIndex '|' . Vector.head

walkPath :: Network -> State -> Writer [Coord] String
walkPath n (dir, pos) = do
  tell [pos]
  case tile pos of
    ' ' -> return []
    '+' -> step turn
    '-' -> step dir
    '|' -> step dir
    node -> (node :) <$> step dir

 where
   tile (Coord j i) = flip Vector.unsafeIndex j . flip Vector.unsafeIndex i $ n
   step dir' = walkPath n (dir', move dir' pos)
   turn = fromJust . find hasPath $ directions \\ [inverse dir]
   hasPath dir =
     case (dir, tile (move dir pos)) of
       (_,      ' ') -> False
       (DLeft,  '-') -> True
       (DRight, '-') -> True
       (DUp,    '|') -> True
       (DDown,  '|') -> True
       _ -> True

main :: IO ()
main = solve parse part1 part2
  where
    parse = Vector.fromList . map Vector.fromList . lines
    part1 = fst . runWriter . (walkPath <$> id <*> start)
    part2 = subtract 1 . length . execWriter . (walkPath <$> id <*> start)
