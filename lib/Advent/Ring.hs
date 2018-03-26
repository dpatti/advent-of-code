{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ViewPatterns #-}

module Advent.Ring where

import qualified Data.Foldable as Foldable
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq

newtype Ring a = Ring { unRing :: Seq a }
  deriving (Show, Foldable)

fromList :: [a] -> Ring a
fromList = Ring . Seq.fromList

toList :: Ring a -> [a]
toList = Foldable.toList . unRing

peek :: Int -> Ring a -> [a]
peek n = Foldable.toList . Seq.cycleTaking n . unRing

skip :: Int -> Ring a -> Ring a
skip n (unRing -> s) =
  Ring . Seq.drop n . Seq.cycleTaking (n + Seq.length s) $ s

skipWhile :: (a -> Bool) -> Ring a -> Ring a
skipWhile f (unRing -> s) =
  Ring
  . Seq.take len
  . Seq.dropWhileL f
  . Seq.cycleTaking (2 * len)
  $ s
 where len = Seq.length s

next :: Int -> Ring a -> ([a], Ring a)
next n ring = (peek n ring, skip n ring)

-- Apparently I decided that the spliced elements go to the end
splice :: Int -> ([a] -> [a]) -> Ring a -> Ring a
splice n f ring =
  let (batch, unRing -> s) = next n ring
   in Ring (Seq.drop n s >< Seq.fromList (f batch))
