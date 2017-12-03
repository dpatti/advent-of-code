module Advent.List where

import Advent.Direction
import Data.List
import Data.Ord

findElem :: Eq a => a -> [a] -> Maybe a
findElem = find . (==)

findElemBy :: Eq b => (a -> b) -> b -> [a] -> Maybe a
findElemBy f elem = find ((== elem) . f)

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast f = foldl' (\res x -> if f x then Just x else res) Nothing

-- high water mark: given an array of items, only emit items that are larger
-- than all previous items
hwm :: Ord a => [a] -> [a]
hwm (x:xs) = x : hwm (filter (> x) xs)

-- eachCons 2 "abcd" == ["ab", "bc", "cd"]
eachCons :: Int -> [a] -> [[a]]
eachCons n xs
  | length xs < n = []
  | otherwise = take n xs : eachCons n (tail xs)

-- eachSlice 2 "abcd" == ["ab", "cd"]
eachSlice :: Int -> [a] -> [[a]]
eachSlice _ [] = []
eachSlice n xs = take n xs : eachSlice n (drop n xs)

mode :: (Eq a, Ord a) => [a] -> a
mode = head . minimumBy (comparing length) . group . sort

-- combinations 2 "abc" == ["ab", "ac", "bc"]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) =
 ((x:) <$> combinations (n - 1) xs) ++ combinations n xs

-- count 2 [1, 2, 3, 4, 3, 2, 1] == 2
count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

-- allPairs [1, 2] == [(1, 1), (1, 2), (2, 1), (2, 2)]
allPairs :: [a] -> [(a, a)]
allPairs xs = map (,) xs <*> xs

-- rotate DLeft 2 [1, 2, 3, 4, 5] == [3, 4, 5, 1, 2]
-- rotate DRight 2 [1, 2, 3, 4, 5] == [4, 5, 1, 2, 3]
rotate :: Direction -> Int -> [a] -> [a]
rotate DLeft n xs = take (length xs) . drop n . cycle $ xs
rotate DRight n xs = take (length xs) . drop (length xs - n) . cycle $ xs
