{-# LANGUAGE NamedFieldPuns #-}

import Advent

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

data Instruction =
  Instruction
    { register :: String
    , effect :: Int -> Int
    , condition :: (String, Int -> Bool)
    }

parseOperator :: Parser (Int -> Int -> Int)
parseOperator =
  (string "inc" $> (+)) <|> (string "dec" $> flip (-))

parseConditional :: Parser (Int -> Int -> Bool)
parseConditional =
  choice [ string "==" $> (==)
         , string "!=" $> (/=)
         , string "<=" $> flip (<=)
         , string ">=" $> flip (>=)
         , string "<"  $> flip (<)
         , string ">"  $> flip (>)
         ]

parseInstruction :: Parser Instruction
parseInstruction = do
  register <- word

  operator <- parseOperator
  space
  amount <- signed space decimal
  space
  let effect = operator amount

  string "if "
  switch <- word
  space
  conditional <- parseConditional
  space
  camount <- signed space decimal
  let condition = (switch, conditional camount)

  return Instruction { register, effect, condition }

compute :: Map String Int -> Instruction -> Map String Int
compute registers Instruction { register, effect, condition } =
  if test (lookup target)
     then update effect register
     else registers
  where
    (target, test) = condition
    lookup reg = Map.findWithDefault 0 reg registers
    -- alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
    update f reg = Map.alter (Just . f . fromMaybe 0) reg registers

largestRegister :: Map String Int -> Int
largestRegister = maximum . Map.elems

main :: IO ()
main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

 where
   parse = map (parseWith parseInstruction) . lines
   solve1 = largestRegister . foldl compute Map.empty
   solve2 = maximum . map largestRegister . tail . scanl compute Map.empty
