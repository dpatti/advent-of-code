import Advent

import Data.Map (Map, (!))
import qualified Data.Map as Map

type Pipe = (Int, [Int])

pipeParser :: Parser Pipe
pipeParser = do
  source <- decimal
  string " <-> "
  dests <- decimal `sepBy` string ", "
  return (source, dests)

connectedTo :: Int -> Map Int [Int] -> Int
connectedTo n pipes = length . nub $ bfsMemo (pipes !) [n]

-- We're doing a lot of extra work here, but it's still fairly fast. The idea is
-- to figure out the number of nodes in the group for each process. So since we
-- had 288 in the 0 group, there should be 288 nodes with a connectivity of 288.
-- By sorting this list, we look at the head and drop that many entries from the
-- list, which allows us to disambiguate, say, two different groups of size 6.
totalGroups :: Map Int [Int] -> Int
totalGroups pipes = tally . sortBy (flip compare) $ connections
  where
    connections = map (`connectedTo` pipes) [0 .. fst (Map.findMax pipes)]

    tally [] = 0
    tally (x:xs) = 1 + tally (drop (x - 1) xs)

main :: IO ()
main = solve parse part1 part2
  where
    parse = Map.fromList . map (parseWith pipeParser) . lines
    part1 = connectedTo 0
    part2 = totalGroups
