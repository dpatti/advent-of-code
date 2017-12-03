import Advent
import Data.Maybe
import Text.Read

rotate' :: Int -> [a] -> [a]
rotate' = rotate DRight

intString :: String -> [Int]
intString = mapMaybe (readMaybe . return)

sumPairs :: [Int] -> [Int] -> Int
sumPairs xs ys = sum (zipWith isPair xs ys)
  where
    isPair x y
      | x == y = x
      | otherwise = 0

sumWheel :: [Int] -> Int
sumWheel xs = sumPairs xs (rotate' 1 xs)

sumAcross :: [Int] -> Int
sumAcross xs = sumPairs xs (rotate' (length xs `div` 2) xs)

main :: IO ()
main = do
  input <- getContents
  print . solve1 $ input
  print . solve2 $ input
 where
   solve1 = sumWheel . intString
   solve2 = sumAcross . intString
