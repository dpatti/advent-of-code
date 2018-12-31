import Advent
import Advent.Ring (Ring)
import qualified Advent.Ring as Ring
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Marble = Int
type Score = Int

rulesParser :: Parser (Int, Int)
rulesParser = do
  players <- integer
  lastMarblePoints <- skipManyTill word integer
  return (players, lastMarblePoints)

cycleRange :: Int -> [Int]
cycleRange n = cycle [1..n]

points :: [Score]
points = unfoldr (Just . turn) (1, Ring.fromList [0])
 where
  turn :: (Marble, Ring Marble) -> (Score, (Marble, Ring Marble))
  turn (nextMarble, marbles)
    | nextMarble `mod` 23 == 0 =
      let (removed, marbles') = Ring.remove . Ring.rewind 7 $ marbles
       in (nextMarble + fromJust removed, (nextMarble + 1, marbles'))
    | otherwise =
      (0, (nextMarble + 1, Ring.insert nextMarble . Ring.skip 2 $ marbles))

main :: IO ()
main = solve parse part1 part2
  where
    parse = parseWith rulesParser
    part1 =
      winningScore <<< tally <<< app <<< (zip . cycleRange) *** (($ points) . take)
    part2 = part1 <<< second (* 100)

    tally = Map.fromListWith (+)
    winningScore = maximum
