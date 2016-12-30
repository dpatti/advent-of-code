{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace

-- Ar only shows up at the end of RHS, which means it must end a segment
-- C never shows up on LHS or in formula
-- Maybe Ar is the key? Seems wrong though
data Element = E | Al | B | Ca | F | H | Mg | N | O | P | Si | Th | Ti | Ar | C | Rn | Y
  deriving (Ord, Eq, Show, Read)
type Molecule = [Element]
data Rule = Rule { from :: Element, to :: Molecule } deriving Show

data Step = Replace Rule
          | Consume Element 
          deriving Show

data Annotation a b = Annotation [a] b deriving Show
type Path = Annotation Step Molecule

instance Functor (Annotation a) where
  fmap f (Annotation as b) = Annotation as (f b)

instance Applicative (Annotation a) where
  pure = return
  af <*> aa = do
    f <- af
    a <- aa
    pure (f a)

instance Monad (Annotation a) where
  return = Annotation []
  (Annotation as b) >>= f = Annotation (as ++ fas) fb
    where (Annotation fas fb) = f b

annotate :: a -> b -> Annotation a b
annotate a = Annotation [a]

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
elementParser = (string "e" *> return E)
            <|> read <$> ((:) <$> chars ['A'..'Z'] <*> many' (chars ['a'..'z']))
    where chars = satisfy . inClass

next :: [Rule] -> Element -> [Molecule]
next rules molecule = map to . filter ((== molecule) . from) $ rules

-- {{n - 3}}
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

annotatedReplace :: [Rule] -> Molecule -> [Path]
annotatedReplace rules molecule = concatMap (replace' molecule) rules
  where
    replace' :: Molecule -> Rule -> [Path]
    replace' [] _ = []
    replace' (el:molecule) rule@(Rule target replacement)
      | el == target = annotate (Replace rule) (replacement ++ molecule) : next
      | otherwise = next

      where
        next = map ((el :) <$>) (replace' molecule rule)

-- {{n - 2}}
substitute :: [Rule] -> [Molecule] -> [Molecule]
substitute rs ms = do
  r <- rs
  let res = concatMap (substituteMolecule r) ms
   in seq (show (length ms) ++ " -> " ++ show (length res)) res

 where
    substituteMolecule :: Rule -> Molecule -> [Molecule]
    substituteMolecule _ [] = []
    substituteMolecule r@(Rule from to) m
      | to == chain = (from : rest) : next
      | otherwise = next

      where
        next = map (head m :) (substituteMolecule r (tail m))
        chain = take (length to) m
        rest = drop (length to) m

    matches :: Molecule -> Molecule -> Bool
    matches [] _ = True
    matches _ [] = False
    matches (x:xs) (y:ys) = (x == y) && matches xs ys

electron = [E]

invalid :: Molecule -> Bool
invalid m = length m > 1 && E `elem` m

success :: Molecule -> Bool
success m = electron == m

each :: (a -> b) -> [a] -> [a]
each f (x:xs) = seq (f x) (x : each f xs)

uniq :: (Ord a) => [a] -> [a]
uniq = Set.toList . Set.fromList

chunk :: (a -> Bool) -> [a] -> [[a]]
chunk _ [] = [empty]
chunk f (x:xs) =
  if f x
     then [x] : rest
     else (x : head rest) : tail rest

 where
   rest = chunk f xs

greedyChunk :: (a -> Bool) -> [a] -> ([a], [a])
greedyChunk f [] = ([], [])
greedyChunk f (x:xs) =
  case greedyChunk f xs of
    ([], r) -> if f x
                  then ([x], r)
                  else ([], x:r)
    (l, r) -> (x:l, r)

pathTo :: [Rule] -> Molecule -> Molecule -> [(Rule, Molecule)]
pathTo rules start end = undefined
  where
    split :: (Molecule, Molecule)
    split = greedyChunk (== Ar) end
    -- allPaths :: [Molecule]
    -- allPaths = iterate (concatMap (next rules)) [start]

main :: IO ()
main = do
  input <- lines <$> getContents
  let rules = parseInput rulesParser . intercalate "\n" . takeWhile (/= "") $ input
  let molecule = parseInput moleculeParser . head . tail . dropWhile (/= "") $ input

  let chunked = chunk (== Ar) molecule

  print . take 5 . annotatedIterate rules $ return [E]

  -- print . pathTo rules [E] $ molecule

  -- print . dig rules $ chunked

  -- print $ search rules molecule
  -- print $ substitute [Rule "X" ["Y"]] [["X", "Y", "Z"]]

  -- print $ replace rules molecule
  -- print . length . nub . replace rules $ molecule
  -- let onlySmall = (< length molecule) . length . fst
  -- let solutions = (["e"], 0) : concatMap (filter onlySmall . step rules) solutions
  -- print $ solutions !! 0
  -- print $ step rules (solutions !! 0)
  -- print $ solutions !! 1

  -- print solutions
  -- print . Data.List.take 10 $ solutions
  -- print . head . filter ((== molecule) . fst) $ solutions
  -- print . head . filter ((== molecule) . fst) $ solutions

  where
    parseInput :: Parser a -> String -> a
    parseInput p = fromRight . parseOnly p . T.pack
    fromRight (Right b) = b

    -- {{n}}
    -- dig :: [Rule] -> [Molecule] -> Int
    -- dig rules chunked = finish . foldl' ((chew .) <$> combine) ([], 0) $ chunked
    --   where
    --     combine :: (Molecule, Int) -> Molecule -> (Molecule, Int)
    --     combine (mol, count) next = traceShow (count, mol, next) (mol <> next, count)

    --     chew :: (Molecule, Int) -> (Molecule, Int)
    --     chew (mol, count) = (reduced, count + iterations)
    --       where
    --         exhausted = exhaust mol
    --         steps = takeWhile (not . any isReduced) exhausted
    --         iterations = traceShowId (length steps)
    --         reduced = fromJust . find isReduced . head . drop iterations $ exhausted

    --         isReduced :: Molecule -> Bool
    --         isReduced els = last els /= Ar

    --     exhaust :: Molecule -> [[Molecule]]
    --     exhaust mol = takeWhile (not . null) . iterate (step rules) $ [mol]

    --     finish :: (Molecule, Int) -> Int
    --     finish (mol, count) = count + length (exhaust mol) - 1

    -- {{n - 1}}
    -- search :: [Rule] -> Molecule -> Int
    -- search rules mol = find . debug . graph $ [mol]
    --     
    --   where
    --     find = countUntil (success . head)
    --     debug = each traceShowId
    --     graph = iterate (step rules)

    --     countUntil :: (a -> Bool) -> [a] -> Int
    --     countUntil pred = length . takeWhile (not . pred)
    
    annotatedIterate :: [Rule] -> Path -> [Path]
    annotatedIterate rules (Annotation steps molecule) = do
      path <- annotatedReplace rules molecule
      [join (Annotation steps path)]

    step :: [Rule] -> [Molecule] -> [Molecule]
    step rules = uniq . filter (not . invalid) . substitute rules

    -- step :: [Rule] -> (Molecule, Int) -> [(Molecule, Int)]
    -- step rules (mol, i) = (, i + 1) <$> replace rules mol
