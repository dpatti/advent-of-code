import Advent
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set

takeWhileUnique :: (Ord a) => [a] -> [a]
takeWhileUnique = loop Set.empty
  where
    loop set (x:xs) =
      if Set.member x set
         then []
         else x : loop (Set.insert x set) xs

rebalance :: [Int] -> [Int]
rebalance xs = zipWith add xs [0..]
  where
    add n i = multiplier i * n + gains i
    multiplier i
      | i == index = 0
      | otherwise = 1
    gains i = whole + part i
    size = length xs
    whole = mem `div` size
    part i
      | ((i - index - 1) `mod` length xs) < (mem `mod` size) = 1
      | otherwise = 0
    (mem, index) = maximumBy order (zip xs [0..])
    order = compare `on` fmap negate

cycleLength :: [[Int]] -> Int
cycleLength xs =
  let repeat = rebalance (last xs)
      start = elemIndex repeat xs
   in length xs - fromJust start

main :: IO ()
main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

 where
   parse = map read . splitOn "\t"
   solve1 = length . takeWhileUnique . iterate rebalance
   solve2 = cycleLength . takeWhileUnique . iterate rebalance
