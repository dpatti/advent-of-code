{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace
import qualified Data.PQueue.Min as MinQueue

data Rule = Rule { from :: Element, to :: [Element] } deriving Show

rulesParser :: Parser [Rule]
rulesParser = ruleParser `sepBy` char '\n'

ruleParser :: Parser Rule
ruleParser = do
  el <- elementParser
  string " => "
  mol <- many1 elementParser
  return $ Rule el mol

rules :: [Rule]
rules = map (parseInput ruleParser)
  [ "Al => ThF"
  , "B => BCa"
  , "B => TiB"
  , "Ca => CaCa"
  , "Ca => PB"
  , "Ca => SiTh"
  , "F => CaF"
  , "F => PMg"
  , "F => SiAl"
  , "H => HCa"
  , "H => NTh"
  , "H => OB"
  , "Mg => BF"
  , "Mg => TiMg"
  , "N => HSi"
  , "O => HP"
  , "O => OTi"
  , "P => CaP"
  , "P => PTi"
  , "Si => CaSi"
  , "Th => ThCa"
  , "Ti => BP"
  , "Ti => TiTi"
  , "e => HF"
  , "e => NAl"
  , "e => OMg" ]

  -- , "H => ORnFAr"
  -- , "N => CRnFAr"
  -- , "O => CRnFYFAr"
  -- , "O => CRnMgAr"
  -- , "O => NRnFAr"
  -- , "P => SiRnFAr"
  -- , "Al => ThRnFAr"
  -- , "B => TiRnFAr"
  -- , "Ca => PRnFAr"
  -- , "Ca => SiRnFYFAr"
  -- , "Ca => SiRnMgAr"
  -- , "H => CRnAlAr"
  -- , "H => CRnFYFYFAr"
  -- , "H => CRnFYMgAr"
  -- , "H => CRnMgYFAr"
  -- , "H => NRnFYFAr"
  -- , "H => NRnMgAr"

data Element = E | Al | B | Ca | F | H | Mg | N | O | P | Si | Th | Ti 
             | C | Y | Ar | Rn
  deriving (Ord, Eq, Show, Read)

newtype Molecule = Molecule { elements :: [Element] }
instance Show Molecule where
  show = mconcat . map show . elements

newtype Section = Section { packed :: Compound }
instance Show Section where
  show sec = mconcat ["{", show (packed sec), "}"]

data Bisection = Bisection { left :: Compound, right :: Compound }
instance Show Bisection where
  show bisec = mconcat ["{", show (left bisec), "|", show (right bisec), "}"]

data Link = MLink Molecule
          | SLink Section
          | BLink Bisection

instance Show Link where
  show (MLink x) = show x
  show (SLink x) = show x
  show (BLink x) = show x

data Compound = Compound { links :: [Link] }

singleton :: Element -> Compound
singleton el = Compound [MLink (Molecule [el])]

instance Show Compound where
  show = mconcat . map show . links

instance Semigroup Compound where
  Compound l <> Compound r = Compound (l ++ r)

instance Monoid Compound where
  mempty = Compound { links = [] }

data Reduction a = Reduction { steps :: Sum Int, value :: a }

incr :: Int -> Reduction a -> Reduction a
incr n reduc = reduc { steps = steps reduc + Sum n }

instance Functor Reduction where
  fmap f reduc = reduc { value = f (value reduc) }

instance Applicative Reduction where
  pure = return
  rf <*> rv = do
    f <- rf
    v <- rv
    return (f v)

instance Monad Reduction where 
  return value = Reduction { steps = mempty, value = value }
  (Reduction steps value) >>= f = 
    let (Reduction steps' value') = f value
     in Reduction { steps = steps + steps', value = value' }

instance (Semigroup a) => Semigroup (Reduction a) where
  a <> b = Reduction { steps = steps a <> steps b
                     , value = value a <> value b }

instance (Monoid a) => Monoid (Reduction a) where
  mempty = Reduction { steps = Sum 0, value = mempty }

reduce :: Compound -> Reduction Compound
reduce = (mconcat <$>) . mapM reduce' . links
  where
    reduce' :: Link -> Reduction Compound
    reduce' (MLink mol) = reduceMol mol
    reduce' (SLink sec) = reduceSec sec
    reduce' (BLink bis) = reduceBis bis

reduceMol :: Molecule -> Reduction Compound
reduceMol mol = return Compound { links = [MLink mol] }

reduceSec :: Section -> Reduction Compound
reduceSec sec = return Compound { links = [SLink sec] }

reduceBis :: Bisection -> Reduction Compound
reduceBis (Bisection left right) = do
  left' <- path (singleton F) left
  right' <- path (singleton F) right

  return Compound { links = [BLink Bisection { left = left', right = right' }] }

path :: Compound -> Compound -> Reduction Compound
path from to = maybe failure success (countSteps from to)
  where
    failure :: Reduction Compound
    failure = return to

    success :: Int -> Reduction Compound
    success steps = incr steps (return from)

countSteps :: Compound -> Compound -> Maybe Int
countSteps from to = countStepsLinks (links from) (links to)

countStepsLinks :: [Link] -> [Link] -> Maybe Int
countStepsLinks [MLink from] [MLink to] =
  listToMaybe $ bruteSteps (elements from) (elements to)
countStepsLinks _ _ = Nothing

bruteSteps :: [Element] -> [Element] -> [Int]
bruteSteps from = replaceAll from
  where
    bruteSteps' steps froms tos
      | froms == tos = [steps]
      | otherwise = [] -- something replacements froms
    replaceAll = undefined

-- -----------------------------------------------------------------------------

compoundParser :: Parser Compound
compoundParser = Compound <$> links
  where
    links =
      many1 $ BLink <$> bisectionParser
          <|> SLink <$> sectionParser
          <|> MLink <$> moleculeParser

bisectionParser :: Parser Bisection
bisectionParser = do
  exactElement Rn
  left <- compoundParser
  exactElement Y
  right <- compoundParser
  exactElement Ar

  return (Bisection left right)

sectionParser :: Parser Section
sectionParser = do
  exactElement Rn
  packed <- compoundParser
  exactElement Ar

  return Section { packed = packed }

moleculeParser :: Parser Molecule
moleculeParser = do
  elements <- basicElementsParser
  return Molecule { elements = elements }

basicElementsParser :: Parser [Element]
basicElementsParser = do
  element <- elementParser
  guard (element `notElem` [Rn, Y, Ar])
  (element :) <$> basicElementsParser <|> return [element]

elementParser :: Parser Element
elementParser = (string "e" *> return E)
            <|> read <$> ((:) <$> chars ['A'..'Z'] <*> many' (chars ['a'..'z']))
    where chars = satisfy . inClass

exactElement :: Element -> Parser Element
exactElement target = do
  element <- elementParser
  guard (element == target)
  return target

-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------

parseInput :: Parser a -> String -> a
parseInput p = fromRight . parseOnly p . T.pack
fromRight (Right b) = b

main :: IO ()
main = do
  input <- lines <$> getContents
  let compound = parseInput compoundParser . head . tail . dropWhile (/= "") $ input

  print compound
  let (Reduction steps compound') = reduce compound
  print steps
  print compound'
