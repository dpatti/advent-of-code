import Data.List
import Data.List.Split
import Data.Ord

type Range = (Int, Int)

parseRange :: String -> Range
parseRange = tup2 . map read . splitOn "-"
  where tup2 [a, b] = (a, b)

minIp = 0
maxIp = 4294967295

-- [(1, 2), (3, 4)] -> [(min, 1), (2, 3), (4, max)]
offset :: [(Int, Int)] -> [(Int, Int)]
offset = offset' minIp
  where
    offset' end [] = [(end, maxIp)]
    offset' prev ((low, high) : xs) = (prev, low - 1) : offset' (max prev (high + 1)) xs

valid :: (Int, Int) -> Bool
valid (a, b) = a <= b

rangeSize :: (Int, Int) -> Int
rangeSize (a, b) = b - a + 1

main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

  where
    parse = map parseRange . lines
    allowed = filter valid . offset . sortBy (comparing fst)
    solve1 = fst . head . allowed
    solve2 = sum . map rangeSize . allowed
