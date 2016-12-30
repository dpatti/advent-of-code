module Advent.Bounded where

bound :: Ord a => a -> a -> a -> a
bound lower upper = max lower . min upper

inBounds :: Ord a => a -> a -> a -> Bool
inBounds lower upper target = target == bound lower upper target
