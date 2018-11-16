{-# LANGUAGE InstanceSigs, NamedFieldPuns #-}
import Advent
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map (Map)

class Implement a where
  clean :: a
  fromChar :: Char -> a
  turn :: a -> Direction -> Direction
  modify :: a -> a

data Simple = Infected | Clean deriving (Eq, Show)

instance Implement Simple where
  clean = Clean

  fromChar :: Char -> Simple
  fromChar '#' = Infected
  fromChar '.' = Clean

  turn :: Simple -> Direction -> Direction
  turn Infected = cwTurn
  turn Clean = ccwTurn

  modify :: Simple -> Simple
  modify Clean = Infected
  modify Infected = Clean

data Evolved = EvoInfected | EvoClean | EvoFlagged | EvoWeakened deriving (Eq, Show)

instance Implement Evolved where
  clean = EvoClean

  fromChar :: Char -> Evolved
  fromChar '#' = EvoInfected
  fromChar '.' = EvoClean

  turn :: Evolved -> Direction -> Direction
  turn EvoClean = ccwTurn
  turn EvoInfected = cwTurn
  turn EvoWeakened = id
  turn EvoFlagged = inverse

  modify :: Evolved -> Evolved
  modify EvoClean = EvoWeakened
  modify EvoWeakened = EvoInfected
  modify EvoInfected = EvoFlagged
  modify EvoFlagged = EvoClean

type GridMap a = Map Coord a

data State a = State
  { grid :: GridMap a
  , direction :: Direction
  , position :: Coord }

toGridMap :: (Implement a) => [String] -> GridMap a
toGridMap statusRows = Map.fromList $ do
  (y, row) <- zip [0..] statusRows
  (x, status) <- zip [0..] row
  return (Coord x y <> offset, fromChar status)

 where
   height = length statusRows
   width = length (head statusRows)
   offset = cmap negate $ Coord (width `div` 2) (height `div` 2)

initialize :: GridMap a -> State a
initialize grid = State { grid, direction = DUp, position = Coord 0 0 }

step :: (Implement a) => State a -> Maybe (a, State a)
step State { grid, direction, position } = Just (modified, state')
  where
    state' =
      State { grid = Map.insert position modified grid
            , direction = direction'
            , position = move direction' position
            }
    current = Map.lookup position grid & fromMaybe clean
    modified = modify current
    direction' = turn current direction

main :: IO ()
main = solve parse part1 part2
  where
    parse = lines
    part1 = go 10000 Infected
    part2 = go tenMillion EvoInfected

    go bursts target = count target . take bursts . unfoldr step . initialize . toGridMap

    tenMillion = 10000000
