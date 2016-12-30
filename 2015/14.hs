{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text hiding (take)
import           Data.List
import qualified Data.Map as Map
import qualified Data.Text as T

type Reindeer = String
data Stat = Stat {
  name :: Reindeer
, speed :: Int
, interval :: Int
, rest :: Int
} deriving Show

type Pos = Int
type Point = Int
data State = Flying Int | Resting Int

fromRight :: Either a b -> b
fromRight (Right b) = b

stat :: Parser Stat
stat = do
  -- Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
  reindeer <- many1 letter
  string " can fly "
  speed <- decimal
  string " km/s for "
  interval <- decimal
  string " seconds, but then must rest for "
  rest <- decimal
  string " seconds."

  return $ Stat reindeer speed interval rest

time :: Int -> Stat -> [Pos]
time s stat = map fst . tail . take (s + 1) . iterate step $ init
  where
    init :: (Pos, State)
    init = (0, Flying (interval stat))
    
    step :: (Pos, State) -> (Pos, State)
    step (pos, Flying 0) = step (pos, Resting (rest stat))
    step (pos, Flying n) = (pos + speed stat, Flying (n - 1))

    step (pos, Resting 0) = step (pos, Flying (interval stat))
    step (pos, Resting n) = (pos, Resting (n - 1))

points :: [Pos] -> [Point]
points pos = map award pos
  where
    par = maximum pos
    award pos | par == pos = 1
              | otherwise  = 0

main :: IO ()
main = do
  stats <- map (parse . T.pack) . lines <$> getContents
  let results = map (time 2503) stats
  -- print $ zip (map name stats) results
  print . map sum . transpose . map points . transpose $ results
  -- print $ maximum results
  where parse = fromRight . parseOnly stat
