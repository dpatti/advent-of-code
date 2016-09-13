{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text
import           Data.Bits
import           Data.Char
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Word
import           Data.List
import qualified Data.Text as T

type Name = String
type Value = Word16

data Op = Unary Name (Value -> Value)
        | Binary Name Name (Value -> Value -> Value)
data Wire = Wire { name :: Name, op :: Op }
type Circuit = Map.Map Name Value

-- evaluate :: Wires -> Name -> Value
-- evaluate wires name = case Map.lookup name wires of
--   Nothing -> error $ "Missing " ++ name
--   Just (Unary s f) -> f (re s)
--   Just (Binary s1 s2 f) -> re s1 `f` re s2
--   where
--     re s = case Map.lookup s wires of
--       Nothing -> read s
--       Just _ -> evaluate wires s

eval :: Circuit -> Wire -> Maybe Value
eval circuit wire = case wire of
  Wire { op = (Unary s f) } -> f <$> find s 
  Wire { op = (Binary s1 s2 f) } -> f <$> find s1 <*> find s2
  where
    find :: Name -> Maybe Value
    find name = if all isDigit name
      then Just (read name)
      else Map.lookup name circuit

construct :: [Wire] -> Circuit
construct = construct' Map.empty []
  where
    construct' :: Circuit -> [Wire] -> [Wire] -> Circuit
    -- Done
    construct' circuit [] [] = circuit
    -- Next iteration
    construct' circuit pass [] = construct' circuit [] pass
    construct' circuit pass (w:ws) = case eval circuit w of
      -- Skip this round
      Nothing -> construct' circuit (w:pass) ws
      Just val -> construct' circuit' pass ws
        where circuit' = Map.insert (name w) val circuit

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

parseWire :: [String] -> Wire
parseWire ts = Wire { name = name, op = op }
  where
    name = last ts
    op = case ts of
      [s, "->", d] -> Unary s id
      [s1, "AND", s2, "->", d] -> Binary s1 s2 (.&.)
      [s1, "OR", s2, "->", d] -> Binary s1 s2 (.|.)
      ["NOT", s, "->", d] -> Unary s complement
      [s, "LSHIFT", n, "->", d] -> Unary s (`shift` read n)
      [s, "RSHIFT", n, "->", d] -> Unary s (`shift` (-(read n)))

main :: IO ()
main = do
  wires <- fmap (parseWire . words) . lines <$> getContents
  let a = getA wires
  print a
  print . getA . map patch $ wires

  where
    getA = fromJust . Map.lookup "a" . construct
    patch w@(Wire name op) = if name == "b"
      then Wire { name = name, op = Unary "956" id }
      else w
