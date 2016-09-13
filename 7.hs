{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text
import           Data.Bits
import           Data.Char
import qualified Data.Map as Map
import           Data.Word
import           Data.List
import qualified Data.Text as T

type Name = String
type Value = Word16
data Source = Name Name | Value Value
data Op = Direct Source
        | And Source Source
        | Or Source Source
        | Not Source
        | Lshift Source Int
        | Rshift Source Int
type Wires = Map.Map String Op
type Values = Map.Map Name Value

evaluate :: Wires -> Name -> Value
evaluate wires name = case Map.lookup name wires of
  Nothing -> error $ "Missing " ++ name
  Just (Direct s) -> val s
  Just (And s1 s2) -> val s1 .&. val s2
  Just (Or s1 s2) -> val s1 .|. val s2
  Just (Not s) -> complement (val s)
  Just (Lshift s n) -> shift (val s) n
  Just (Rshift s n) -> shift (val s) (-n)
  where
    val (Name s) = evaluate wires s
    val (Value n) = n

graphEval :: Values -> [(Name, Op)] -> Values
graphEval = undefined

-- wireParser :: Parser (Name, Op)
-- wireParser = do
--   op <- opParser
--   string " -> "
--   name <- nameParser
--   return (name, op)
-- 
--   where
--     nameParser = many1 anyChar

-- parseWire :: String -> (Name, Op)
-- parseWire input = case parseOnly wireParser (T.pack input) of
--   (Left e) -> error e
--   (Right v) -> v

parseWire :: [String] -> (Name, Op)
parseWire input = (Name, parseOp) <*> split
  where
    split = ("a", "b")

    parseOp [s] = Direct (source s)
    parseOp [s1, "AND", s2] = And (source s1) (source s2)
    parseOp [s1, "OR", s2] = Or (source s1) (source s2)
    parseOp ["NOT", s] = Not (source s)
    parseOp [s, "LSHIFT", n] = Lshift (source s) (read n)
    parseOp [s, "RSHIFT", n] = Rshift (source s) (read n)

    source s
      | all isAlpha s = Name s
      | otherwise = Value (read s)

main :: IO ()
main = do
  wires <- fmap (parseWire . words) . lines <$> getContents
  -- let sources = filter isSource wires
  -- let known = Map.fromList . map (\(n, Source v) -> (n, v)) $ sources 
  -- let evaluated = graphEval sources wires

  print "A"


  -- print $ evaluate wires "ga"
  -- print $ Map.mapWithKey (\k a -> evaluate wires k) wires
