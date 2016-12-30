{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Debug.Trace

-- data Circle = Circle { elves :: Seq (Int, Int) } deriving Show
type Circle = Seq (Int, Int)

-- highest :: Ord k => Map k a -> k
-- highest = maximum . Map.keys
-- 
-- findNext :: Map Int Int -> Int -> Int
-- findNext elves t = findNext' (t + 1)
--   where
--     findNext' i
--       | i `Map.member` elves = i
--       | otherwise = findNext' (rotate elves i)
-- 
-- rotate :: Map Int Int -> Int -> Int
-- rotate elves t
--   | t + 1 > highest elves = 1
--   | otherwise = t + 1

-- steal :: Circle -> Circle
-- steal c@Circle { elves, turn } = next $ flip execState elves $ do
--   gifts <- gets (Map.lookup turn)
-- 
--   unless (isNothing gifts) $ do
--     presents <- gets (Map.! nextElf)
--     modify (Map.delete nextElf)
--     modify (Map.adjust (+ presents) turn)
-- 
--   return ()
-- 
--   where
--     nextElf = findNext elves turn
--     next elves = Circle { elves = elves
--                         , turn = rotate elves (nextElf - 1) }

stealAdj :: Circle -> Circle
stealAdj (Seq.viewl -> ((turn, p) Seq.:< (Seq.viewl -> (_, p') Seq.:< circle))) =
  circle Seq.|> (turn, p + p')

stealAcr :: Circle -> Circle
stealAcr (Seq.viewl -> (turn, p) Seq.:< rest) =
  before <> after Seq.|> (turn, p + p')
 where
   target =
     case Seq.length rest of
       n | even n -> n `div` 2
         | odd n -> (n + 1) `div` 2
   before = Seq.take (target - 1) rest
   after = Seq.drop target rest
   (_, p') = (0,0)

initCircle :: Int -> Circle
initCircle n = Seq.fromList $ zip [1..n] (repeat 1)

main = do
  input <- parse <$> getContents
  -- print . solve1 $ input
  print . solve2 $ input

 where
   parse = read
   solve1 = find ((== 1) . Seq.length) . iterate stealAdj . initCircle
   solve2 = find ((== 1) . Seq.length) . iterate stealAcr . initCircle
