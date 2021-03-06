module Advent.List where

import Advent.Direction
import Data.List
import Data.Ord
import Data.Map (Map, fromListWith)
import qualified Data.Set as Set

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

-- mode [1,2,2,1,2] == 2
mode :: (Eq a, Ord a) => [a] -> a
mode = head . maximumBy (comparing length) . group . sort

-- freqs [1,2,2,1,2] = fromList [(1,2),(2,3)]
freqs :: (Ord a) => [a] -> Map a Int
freqs = fromListWith (+) . map (flip (,) 1)

-- chunks 2 [1, 2, 3, 4, 5] == [[1, 2], [3, 4], [5]]
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : chunks n rest

-- combinations 2 "abc" == ["ab", "ac", "bc"]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) =
 ((x:) <$> combinations (n - 1) xs) ++ combinations n xs

-- countBy even [1, 2, 3, 4, 5] == 2
countBy :: (a -> Bool) -> [a] -> Int
countBy f = length . filter f

-- count 2 [1, 2, 3, 4, 3, 2, 1] == 2
count :: (Eq a) => a -> [a] -> Int
count x = countBy (== x)

-- allPairs [1, 2] == [(1, 1), (1, 2), (2, 1), (2, 2)]
allPairs :: [a] -> [(a, a)]
allPairs xs = map (,) xs <*> xs

-- rotate DLeft 2 [1, 2, 3, 4, 5] == [3, 4, 5, 1, 2]
-- rotate DRight 2 [1, 2, 3, 4, 5] == [4, 5, 1, 2, 3]
rotate :: Direction -> Int -> [a] -> [a]
rotate DLeft n xs = take (length xs) . drop n . cycle $ xs
rotate DRight n xs = take (length xs) . drop (length xs - n) . cycle $ xs

-- churn is intended to take a list of elements and gives you the ability to
-- look at two elements at a time and decide what to push into the output list,
-- what to push on top of the input list, and ultimately whether or not the
-- computation should continue.
--
-- these are equivalent to `id`:
--   churn (\cont x y -> x : y : cont [])
--   churn (\cont x y -> x : cont [y])
churn :: (([a] -> [a]) -> a -> a -> [a]) -> [a] -> [a]
churn f (x:y:xs) = f (churn f . (++ xs)) x y
churn _ end = end

-- Sort and deduplicate
sortNub :: (Ord a) => [a] -> [a]
sortNub = Set.toList . Set.fromList
