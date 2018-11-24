import Advent
import Data.List.Split
import Data.Maybe

row :: String -> [Int]
row = map read . splitOn "\t"

division :: [Int] -> Int
division = head . mapMaybe divisible . combinations 2
  where
    divisible [a, b]
      | a `mod` b == 0 = Just (a `div` b)
      | b `mod` a == 0 = Just (b `div` a)
      | otherwise = Nothing

checksum :: [Int] -> Int
checksum xs = maximum xs - minimum xs

main :: IO ()
main = do
  input <- getContents
  print . solve1 $ input
  print . solve2 $ input
 where
   solve1 = sum . map (checksum . row) . lines
   solve2 = sum . map (division . row) . lines
