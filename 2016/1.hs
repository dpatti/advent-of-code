import qualified Data.Set as Set

data Turn = L | R deriving (Show, Read)
data Inst = Inst { turn :: Turn, steps :: Int }
type Coord = (Int, Int)
data Direction = N | S | E | W deriving Show

data State = State { dir :: Direction, pos :: Coord } deriving Show

face :: Turn -> Direction -> Direction
face L N = W
face L W = S
face L S = E
face L E = N

face R N = E
face R E = S
face R S = W
face R W = N

walk :: Direction -> Int -> Coord -> Coord
walk N steps (x, y) = (x, y - steps)
walk S steps (x, y) = (x, y + steps)
walk E steps (x, y) = (x + steps, y)
walk W steps (x, y) = (x - steps, y)

travel :: State -> Inst -> State
travel (State dir pos) (Inst turn steps) = State dir' pos'
  where
    dir' = face turn dir
    pos' = walk dir' steps pos

travel' :: State -> [Inst] -> [Coord]
travel' _ [] = []
travel' (State dir pos) (Inst turn steps : xs) = poss ++ travel' end xs
  where
    poss = init $ scanl (\c n -> walk dir' 1 c) pos [1..steps]
    end = State dir' pos'
    dir' = face turn dir
    pos' = walk dir' steps pos

firstRepeat :: (Eq a, Ord a) => [a] -> a
firstRepeat = go Set.empty
  where
    go visited (x:xs)
      | x `elem` visited = x
      | otherwise = go (Set.insert x visited) xs

main = do
  input <- parse <$> getContents
  print $ foldl travel (State N (0, 0)) input
  print $ firstRepeat $ travel' (State N (0, 0)) input

  where
    parse = map pair . words . filter (/= ',')

    pair :: String -> Inst
    pair (turn:steps) = Inst (read [turn]) (read steps)
