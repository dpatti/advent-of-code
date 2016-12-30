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
import System.Random.Shuffle

data Element = E | Al | B | Ca | F | H | Mg | N | O | P | Si | Th | Ti | Ar | C | Rn | Y
  deriving (Ord, Eq, Show, Read)
type Molecule = [Element]
data Rule = Rule { from :: Element, to :: Molecule } deriving Show

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

reduce :: [Rule] -> Molecule -> Maybe Int
reduce rules = reduce' 0
  where
    reduce' steps [E] = return steps
    reduce' steps mol = do
      -- traceM $ "Middle at " ++ show mol
      case substituteFirst mol of
        Nothing -> do
          -- traceM $ "Ended at " ++ show mol
          Nothing
        (Just mol') -> reduce' (steps + 1) mol'

    substituteFirst :: Molecule -> Maybe Molecule
    substituteFirst mol = listToMaybe $ do
      rule <- rules
      -- traceM $! show rule
      -- traceM $! show (to rule) ++ " infix of " ++ show mol
      guard (to rule `isInfixOf` mol)
      return $! substitute rule mol

    substitute :: Rule -> Molecule -> Molecule
    substitute _ [] = []
    substitute r@(Rule from to) m
      | to == chain = from : rest
      | otherwise = next

      where
        next = head m : substitute r (tail m)
        chain = take (length to) m
        rest = drop (length to) m

main :: IO ()
main = do
  input <- lines <$> getContents
  let rules = parseInput rulesParser . intercalate "\n" . takeWhile (/= "") $ input
  let molecule = parseInput moleculeParser . head . tail . dropWhile (/= "") $ input

  foreverResults <- forM [1..10] $ \i -> do
    rules' <- shuffleM rules
    -- print rules'
    let result = reduce rules' molecule
    putStrLn $ show i ++ " " ++ show result
    return result

  print . head . catMaybes $ foreverResults

  where
    parseInput :: Parser a -> String -> a
    parseInput p = fromRight . parseOnly p . T.pack
    fromRight (Right b) = b
