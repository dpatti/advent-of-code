import Data.List

data Tile = Trap | Safe deriving Eq

instance Show Tile where
  show Trap = "^"
  show Safe = "."

type Row = [Tile]

parseRow :: String -> Row
parseRow = map parseTile
  where
    parseTile '.' = Safe
    parseTile '^' = Trap

nextRow :: Row -> Row
nextRow row = do
  pairs <- pairs3 row
  return $ case pairs of
    (Trap, Trap, Safe) -> Trap
    (Safe, Trap, Trap) -> Trap
    (Trap, Safe, Safe) -> Trap
    (Safe, Safe, Trap) -> Trap
    _ -> Safe

-- One == trap
-- Its left and center tiles are traps, but its right tile is not.
-- Its center and right tiles are traps, but its left tile is not.
-- Only its left tile is a trap.
-- Only its right tile is a trap.

pairs3 :: Row -> [(Tile, Tile, Tile)]
pairs3 row = map tup3 . eachCons 3 $ [Safe] ++ row ++ [Safe]
  where
    tup3 [a, b, c] = (a, b, c)

eachCons :: Int -> [a] -> [[a]]
eachCons n xs
  | length xs < n = []
  | otherwise = take n xs : eachCons n (tail xs)

pretty :: [Row] -> String
pretty = intercalate "\n" . map (intercalate "" . map show)

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

main = do
  input <- parse <$> getContents
  -- putStrLn . pretty . solve1 $ input
  print . solve1 $ input
  print . solve2 $ input

 where
   parse = parseRow . head . lines
   solve1 = sum . map (count Safe) . take 40 . iterate nextRow
   solve2 = sum . map (count Safe) . take 400000 . iterate nextRow
