{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.List
import qualified Data.Text as T

type Value = Int

fromRight (Right a) = a

parseInput :: String -> [Value]
parseInput = fromRight . parseOnly parser . T.pack
  where
    parser = do
      name <- many1 letter
      char ':'
      space
      values `sepBy1` string ", "
      
    values = do
      many1 letter
      space
      signed decimal

ranges :: Int -> Int -> Int -> [[Int]]
ranges 0 min max = [[]]
ranges n min max = do
  v <- [min..max]
  map (v:) $ ranges (n - 1) min max

isValid :: [[Int]] -> [Int] -> Bool
isValid valueMatrix counts = correctAmount && calorieMatch
  where
    correctAmount = teaspoons == sum counts
    calorieMatch = calories == (sum . map (uncurry (*)) $ zip counts caloriePer)
    caloriePer = map last valueMatrix

score :: [[Int]] -> [Int] -> Int
score valueMatrix counts =
  product
  . map (max 0 . sum)
  . transpose
  . map tally
  $ zip valueMatrix counts

  where
    tally (values, count) = map (* count) values

teaspoons = 100
calories = 500

main :: IO ()
main = do
  ingredients <- map parseInput . lines <$> getContents
  let calorieFree = map init ingredients
  print . maximum . map (score calorieFree) . filter (isValid ingredients) $ ranges (length ingredients) 0 teaspoons
