import Data.Char
import qualified Data.Map as Map
import Data.List

type Neighborhood = Map.Map (Int, Int) Int

type Command = (Int, Int)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

parse :: Char -> Command
parse '^' = (0, -1)
parse 'v' = (0,  1)
parse '<' = (-1, 0)
parse '>' = ( 1, 0)
parse c = error ("What is this? >" ++ [c] ++ "<")

navigate :: Neighborhood -> [Command] -> Neighborhood
navigate n = fst . foldl step (n, start) . ((0,0):)
  where
    start = (0, 0)
    step (n, (x, y)) (dx, dy) = (Map.alter visit (x+dx, y+dy) n, (x+dx, y+dy))
    visit Nothing = Just 1
    visit (Just v) = Just (v + 1)

visited :: Neighborhood -> Int
visited = length . filter (>0) . map snd . Map.toList 

withIndexes :: [a] -> [(a, Int)]
withIndexes xs = zip xs nats
  where nats = 1 : map (+1) nats

main :: IO ()
main = do
  input <- filter (not . isSpace) <$> getContents
  let route = map parse input

  print . visited . navigate Map.empty $ route

  let partitioned = partition (even . snd) . withIndexes $ route
  let (santaRoute, roboRoute) = both (map fst) partitioned
  let first = navigate Map.empty santaRoute
  print . visited . navigate first $ roboRoute
