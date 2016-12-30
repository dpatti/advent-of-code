{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

data Action = TurnOn | TurnOff | Toggle deriving Show
type Coord = (Int, Int)
data Range = Range { from :: Coord, to :: Coord } deriving Show
data Command = Command Action Range deriving Show

commandParser :: Parser Command
commandParser = do
  action <- actionParser
  string " "
  range <- rangeParser
  return $ Command action range

  where
    actionParser :: Parser Action
    actionParser = (string "turn on" >> return TurnOn)
               <|> (string "turn off" >> return TurnOff)
               <|> (string "toggle" >> return Toggle)

    rangeParser :: Parser Range
    rangeParser = do
      c1 <- coordParser
      string " through "
      c2 <- coordParser
      return $ Range c1 c2

    coordParser :: Parser Coord
    coordParser = do
      d1 <- decimal
      string ","
      d2 <- decimal
      return (d1, d2)

parseCommand :: String -> Command
parseCommand input = case parsed of
  Left e -> error $ input ++ " gave " ++ e
  Right cs-> cs
  where
    parsed = parseOnly commandParser . BS.pack $ input

expandRange :: Range -> [Coord]
expandRange (Range (x1, y1) (x2, y2)) = do
  x <- [x1..x2]
  y <- [y1..y2]
  [(x, y)]

inRange :: Range -> Coord -> Bool
inRange (Range (x1, y1) (x2, y2)) (x, y) =
  x1 <= x && x <= x2 && y1 <= y && y <= y2

run :: [Command] -> Coord -> Int
run commands coord = sum . map amt $ commands
  where
    amt (Command action range) =
      if inRange range coord
      then worth action
      else 0

    worth TurnOn  = 1
    worth TurnOff = -1
    worth Toggle  = 2

main :: IO ()
main = do
  commands <- fmap parseCommand . lines <$> getContents
  print . sum . map (run commands) . expandRange $ Range (0, 0) (999, 999)
