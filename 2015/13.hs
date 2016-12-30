{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.List
import qualified Data.Map as Map
import qualified Data.Text as T

type Person = String
type Relationship = ((Person, Person), Int)

fromRight :: Either a b -> b
fromRight (Right b) = b

person :: Parser Person
person = many1 letter

happiness :: Parser Int
happiness = do
  mag <- (string "gain" >> return 1) <|> (string "lose" >> return (-1))
  space
  n <- decimal
  string " happiness units"

  return $ mag * n

relationship :: Parser Relationship
relationship = do
  p1 <- person
  string " would "
  h <- happiness
  string " by sitting next to "
  p2 <- person

  return ((p1, p2), h)

total :: Map.Map (Person, Person) Int -> [Person] -> [Int]
total rmap [_] = []
total rmap (p1:p2:ps) = cost p1 p2 : total rmap (p2:ps)
  where cost p1 p2 = sum . map (rmap Map.!) $ [(p1, p2), (p2, p1)]

main :: IO ()
main = do
  relationships <- map (fromRight . parseOnly relationship . T.pack) . lines <$> getContents
  let rmap = Map.fromList relationships
  let people = nub . map (fst . fst) $ relationships

  print . maximum . map (cutWeakest . total rmap . circularize) . permutations $ people

  where
    circularize xs = last xs : xs
    cutWeakest = sum . tail . sort
