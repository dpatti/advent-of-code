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

class Reducible t where
  steps :: t -> Int

data Element = E | Al | B | Ca | F | H | Mg | N | O | P | Si | Th | Ti 
             | C | Y | Ar | Rn
  deriving (Ord, Eq, Show, Read)

newtype Molecule = Molecule { elements :: [Element] }
instance Reducible Molecule where
  steps mol = length (elements mol)
instance Show Molecule where
  show = mconcat . map show . elements

newtype Section = Section { packed :: Compound }
instance Reducible Section where
  steps section = steps (packed section) + 1
instance Show Section where
  show sec = mconcat ["{", show (packed sec), "}"]

data Bisection = Bisection { left :: Compound, right :: Compound }
instance Reducible Bisection where
  steps bis = steps (left bis) + steps (right bis) + 1
instance Show Bisection where
  show bisec = mconcat ["{", show (left bisec), "|", show (right bisec), "}"]

data Link = MLink Molecule
          | SLink Section
          | BLink Bisection
instance Reducible Link where
  steps (MLink r) = steps r
  steps (SLink r) = steps r
  steps (BLink r) = steps r
instance Show Link where
  show (MLink r) = show r
  show (SLink r) = show r
  show (BLink r) = show r

data Compound = Compound { links :: [Link] }
instance Reducible Compound where
  steps comp = (sum . map steps $ links comp) - 1
instance Show Compound where
  show = mconcat . map show . links

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

main :: IO ()
main = do
  input <- lines <$> getContents
  let compound = parseInput compoundParser . head . tail . dropWhile (/= "") $ input

  print compound
  print (steps compound)

  where
    parseInput :: Parser a -> String -> a
    parseInput p = fromRight . parseOnly p . T.pack
    fromRight (Right b) = b
