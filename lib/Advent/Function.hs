{-# LANGUAGE BangPatterns #-}

module Advent.Function where

-- You can use this to declare instances (e.g., Ord) with
-- compare = compare `with` comparator
-- to delegate comparison to an inner value
with :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
with f g a b = f (g a) (g b)

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f !x = iterateN (n - 1) f (f x)
