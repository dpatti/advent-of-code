{-# LANGUAGE DeriveFoldable #-}

module Advent.Ring where

import Advent.Prelude

newtype Ring a = Ring [a] deriving (Show, Foldable)

fromList :: [a] -> Ring a
fromList = Ring

toList :: Ring a -> [a]
toList (Ring xs) = xs

next :: Int -> Ring a -> ([a], Ring a)
next n (Ring xs) =
  (take n (cycle xs), Ring (cycle xs & drop n & take (length xs)))

skip :: Int -> Ring a -> Ring a
skip n = snd . next n

peek :: Int -> Ring a -> [a]
peek n = fst . next n

splice :: Int -> ([a] -> [a]) -> Ring a -> Ring a
splice n f ring =
  let (batch, Ring xs) = next n ring
   in fromList (take (length xs - n) xs ++ f batch)

