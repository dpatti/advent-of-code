{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Attoparsec.Text

data Sue = Sue { n :: Int, traits :: [Trait] } deriving Show
data Trait = Trait String Int deriving Show
type Clues = Map.Map String (Int -> Bool)

fromRight (Right b) = b

parseSue :: String -> Sue
parseSue = fromRight . parseOnly sue . T.pack
  where
    sue = do
      string "Sue "
      n <- decimal
      char ':'
      space
      traits <- trait `sepBy1` string ", "
      return $ Sue n traits

    trait = do
      id <- many1 letter
      char ':'
      space
      count <- decimal
      return $ Trait id count

matchSue :: Sue -> Bool
matchSue sue = all matchClue (traits sue)
  where
    matchClue (Trait id val) = target Map.! id $ val

target :: Clues
target = Map.fromList [
    ("children", (==3))
  , ("cats", (>7))
  , ("samoyeds", (==2))
  , ("pomeranians", (<3))
  , ("akitas", (==0))
  , ("vizslas", (==0))
  , ("goldfish", (<5))
  , ("trees", (>3))
  , ("cars", (==2))
  , ("perfumes", (==1))
  ]

main :: IO ()
main = do
  sues <- map parseSue . lines <$> getContents
  print . filter matchSue $ sues
