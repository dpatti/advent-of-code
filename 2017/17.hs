{-# LANGUAGE BangPatterns, NamedFieldPuns #-}

import Advent

initial :: Ring Int
initial = fromList [0]

step :: Int -> (Int, Ring Int) -> (Int, Ring Int)
step numSteps (nextInsert, ring) = (nextInsert + 1, ring')
  where
    ring' = splice 0 (const [nextInsert]) . skip numSteps $ ring

-- We need strictness annotations both on [elemOne] here AND on [nextInsert]
-- below because the values aren't used until the final result, and otherwise we
-- would be building up a chain of fifty million thunks.
data FakeRing = FakeRing { elemOne :: !(Maybe Int)
                         , size :: Int
                         , pos :: Int } deriving Show

initial2 :: FakeRing
initial2 = FakeRing { elemOne = Nothing, size = 1, pos = 0 }

step2 :: Int -> (Int, FakeRing) -> (Int, FakeRing)
step2 numSteps (!nextInsert, FakeRing { elemOne, size, pos }) =
  (nextInsert + 1, fakeRing')

 where
   size' = size + 1
   pos' = ((pos + numSteps) `mod` size) + 1
   elemOne' = if pos' == 1 then Just nextInsert else elemOne

   fakeRing' = FakeRing { elemOne = elemOne', size = size', pos = pos' }

main :: IO ()
main = solve parse part1 part2
  where
    fiftyMillion = 50 * 1000 * 1000
    parse = read
    part1 = peek 1 . snd . flip (iterateN 2017) (1, initial) . step
    part2 = snd . flip (iterateN fiftyMillion) (1, initial2) . step2
