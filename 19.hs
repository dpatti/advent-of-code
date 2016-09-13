{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text hiding (takeWhile)
import Data.List
import Data.Monoid
import qualified Data.Text as T

type Element = String
type Molecule = [Element]
data Rule = Rule Element Molecule deriving Show

rulesParser :: Parser [Rule]
rulesParser = ruleParser `sepBy` char '\n'

ruleParser :: Parser Rule
ruleParser = do
  el <- elementParser
  string " => "
  mol <- moleculeParser
  return $ Rule el mol

moleculeParser :: Parser Molecule
moleculeParser = many1 elementParser

elementParser :: Parser Element
elementParser = (T.unpack <$> string "e")
            <|> (:) <$> chars ['A'..'Z'] <*> many' (chars ['a'..'z'])
    where chars = satisfy . inClass

-- replace :: [Rule] -> Molecule -> [Molecule]
-- replace rule molecule = do
--   target <- molecule
--   (:) [target] $ do
--     (Rule replacer replacement) <- rule
--     guard $ replacer == target && False
--     [replacement]

replace :: [Rule] -> Molecule -> [Molecule]
replace rules molecule = concatMap (replace' molecule) rules
  where
    replace' :: Molecule -> Rule -> [Molecule]
    replace' [] _ = []
    replace' (el:molecule) rule@(Rule target replacement)
      | el == target = (replacement ++ molecule) : map (el :) (replace' molecule rule)
      | otherwise = map (el :) (replace' molecule rule)

main :: IO ()
main = do
  input <- lines <$> getContents
  let rules = parseInput rulesParser . intercalate "\n" . takeWhile (/= "") $ input
  let molecule = parseInput moleculeParser . concat . tail . dropWhile (/= "") $ input
  print $ length molecule

  -- print $ replace rules molecule
  -- print . length . nub . replace rules $ molecule
  let onlySmall = (< length molecule) . length . fst
  let solutions = (["e"], 0) : concatMap (filter onlySmall . step rules) solutions
  --print $ solutions !! 0
  --print $ step rules (solutions !! 0)
  --print $ solutions !! 1

  -- print solutions
  -- print . Data.List.take 10 $ solutions
  print . head . filter ((== molecule) . fst) $ solutions
  -- print . head . filter ((== molecule) . fst) $ solutions

  where
    parseInput :: Parser a -> String -> a
    parseInput p = fromRight . parseOnly p . T.pack
    fromRight (Right b) = b

    step :: [Rule] -> (Molecule, Int) -> [(Molecule, Int)]
    step rules (mol, i) = (, i + 1) <$> replace rules mol
