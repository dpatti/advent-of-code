import Advent
import Data.List

isValid :: [String] -> Bool
isValid ws = length (nub ws) == length ws

main :: IO ()
main = do
  input <- getContents
  print . solve1 $ input
  print . solve2 $ input
 where
   solve1 = countBy isValid . map words . lines
   solve2 = countBy isValid . map (map sort . words) . lines
