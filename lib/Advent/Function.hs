{-# LANGUAGE BangPatterns #-}

module Advent.Function where

import Advent.List

-- iterate and return the nth result
iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f !x = iterateN (n - 1) f (f x)

-- iterate until two consecutive values are the same
iterateFix :: Eq a => (a -> a) -> a -> [a]
iterateFix f = churn test . iterate f
  where
    test cont x y
      | x == y = [x]
      | otherwise = x : cont [y]
