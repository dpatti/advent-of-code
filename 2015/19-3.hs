{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace
import qualified Data.PQueue.Min as MinQueue

-- the problem right now is mostly this: we have it trying to brute through
-- chains of ...Ca,Rn,Ca,F,Ar,Y, which is impossible since nothing ends in Y.
-- this might be something to special case, but might not. it means that it took
-- a greedy approach to consume the element after the Y but that did more harm
-- than good
--
-- of course, you would expect that it could backtrace and fix that, but since
-- the brute forcing is limited to 1 more than the non-section length of the
-- molecule tail, it might not be able to plan far enough ahead to deal with
-- a section in the middle. is my idea of a size limit just completely wrong?
-- why do you have to start matching the next section with exactly one element?
--
-- and if that's the case, how do we do this in decent enough time? that
-- basically unbounds the problem completely. is something wrong with my BFS
-- search? i guess the bfs search only applies to the last element, so it will
-- greedily take the best one it can find, remove that element, and then spend
-- however long it wants on what is left, even if it is impossible. not sure how
-- to rectify this given our "right-prioritized" world.

traceAnnotated :: Show a => String -> a -> a
traceAnnotated prefix a = trace (prefix ++ " " ++ show a) a

traceMAnnotated :: (Show a, Monad m) => String -> a -> m ()
traceMAnnotated prefix a = traceM (prefix ++ " " ++ show a)

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

initial :: [a] -> [a]
initial [] = []
initial [_] = []
initial (x:xs) = x : initial xs

-- -----------------------------------------------------------------------------
-- section logic

data Section = Section { before :: Molecule, packed :: Molecule }
  deriving (Show, Eq, Ord)

type SectionIndex = [(Element, Section)]

flattenSection :: Section -> (Molecule, Molecule)
flattenSection (Section before packed) = (before, flattened)
  where
    flattened = concat [ [Rn]
                       , packed
                       , [Ar] ]

-- Can we create a section out of the tail of a molecule?
tailSection :: Molecule -> Maybe Section
tailSection mol =
  case reverse mol of
    (Ar:tail) -> throughRn empty tail
    _ -> Nothing

  where
    throughRn :: Molecule -> Molecule -> Maybe Section
    throughRn _ [] = Nothing
    -- Must deal with packed sections
    throughRn packed (Ar:tail) = do
      innerSection <- throughRn empty tail
      let (before', innerFlattenedSection) = flattenSection innerSection
      throughRn (innerFlattenedSection ++ packed) (reverse before')
    throughRn packed (Rn:before) = Just Section { before = reverse before, packed = packed }
    throughRn packed (p:tail) = throughRn (p:packed) tail

-- Given the rules, what sections are available to us?
indexSections :: [Rule] -> SectionIndex
indexSections = mapMaybe toSection
  where
    toSection :: Rule -> Maybe (Element, Section)
    toSection (Rule from to) = do
      section <- tailSection to
      return (from, section)

-- -----------------------------------------------------------------------------
-- match logic

data MatchAttempt = MatchAttempt { steps :: Int
                                 , fromExcess :: Molecule
                                 , toExcess :: Molecule }
                                 deriving (Show, Eq)

priority :: MatchAttempt -> (Int, Int)
priority (MatchAttempt steps fromExcess toExcess) =
  (steps, length toExcess ^ 2 - length fromExcess ^ 2)

instance Ord MatchAttempt where
  compare a b = compare (priority a) (priority b)

type MatchQueue = MinQueue.MinQueue MatchAttempt

exactMatch :: Int -> MatchAttempt
exactMatch steps = MatchAttempt { steps = steps
                                , fromExcess = empty
                                , toExcess = empty }

-- really should use a reader here
stepsToFullMatch :: [Rule] -> SectionIndex -> Molecule -> Molecule -> Maybe Int
stepsToFullMatch rules sections = countSteps

  where
    countSteps :: Molecule -> Molecule -> Maybe Int
    -- countSteps [] [] = return 0
    -- countSteps _ [] = empty
    -- countSteps [] _ = empty
    countSteps from to = do
      traceM $ "matching from " ++ show from ++ " to " ++ show to
      let initialMatch = MatchAttempt { steps = 0
                                      , fromExcess = from
                                      , toExcess = to }
      listToMaybe $ do
        matchAttempt <- exploreMatches (MinQueue.singleton initialMatch)
        traceM $ "explored match " ++ show matchAttempt
        case matchAttempt of
          (MatchAttempt steps [] []) -> return steps
          _ -> empty

      where
        exploreMatches :: MatchQueue -> [MatchAttempt]
        exploreMatches attempts =
          case MinQueue.minView attempts of
            Nothing -> []
            Just (attempt, queue) -> attempt : exploreMatches (nextAttemptsFrom attempt queue)
          where
            nextAttemptsFrom :: MatchAttempt -> MatchQueue -> MatchQueue
            nextAttemptsFrom attempt queue = foldr enqueue queue (exploreAttempt attempt)
              where
                enqueue :: MatchAttempt -> MatchQueue -> MatchQueue
                enqueue attempt queue =
                  trace ("inserting " ++ show attempt ++ " into queue") $
                    MinQueue.insert attempt queue

            exploreAttempt :: MatchAttempt -> [MatchAttempt]
            exploreAttempt match@(MatchAttempt steps' from' to') = do
              -- tryMatch (and everything?) should probably take an attempt
              nextMatch <- tryMatch from' to'
              return nextMatch { steps = steps nextMatch + steps' }

        -- MatchAttempt steps from' to' <- tryMatch from to
        -- remainingSteps <- maybeToList (countSteps from' to')
        -- return (steps + remainingSteps)

    tryMatch :: Molecule -> Molecule -> [MatchAttempt]
    tryMatch [] _ = empty
    tryMatch _ [] = empty
    tryMatch from to
      | from == to = return (exactMatch 0)
      | otherwise = do
        traceM $ "tryMatch from=" ++ show from ++ "; to=" ++ show to
        -- See if we can section match or have to brute force
        case tailSection to of
          Nothing -> bruteMatch from to
          (Just toSection) -> sectionMatch from toSection

    bruteMatch :: Molecule -> Molecule -> [MatchAttempt]
    bruteMatch [] [] = return (exactMatch 0)
    bruteMatch [] _ = empty
    bruteMatch from to = do
      (Rule element replacement) <- rules
      guard (element == last from)
      let replacedFrom = initial from ++ replacement
      guard (length replacedFrom <= length to)
      guard (last replacedFrom == last to)
      return MatchAttempt { steps = 1
                          , fromExcess = initial replacedFrom
                          , toExcess = initial to }

      where
        bruteMatch' :: [Molecule] -> Int -> [MatchAttempt]
        bruteMatch' [] _
          | null to = return (exactMatch 0)
          | otherwise = empty
        bruteMatch' froms steps = theseMatches
          where
            theseMatches :: [MatchAttempt]
            theseMatches = do
              -- traceMAnnotated "brute matching from one of" froms
              from <- froms
              -- traceMAnnotated "potential match" from
              -- traceMAnnotated "potential to   " to
              guard (last from == last to)
              return MatchAttempt { steps = steps
                                  , fromExcess = initial from
                                  , toExcess = initial to }

            moreMatches :: [MatchAttempt]
            moreMatches = bruteMatch' nextMolecules (steps + 1)

            nextMolecules = do
              replacedMolecule <- replacedMolecules
              guard (length replacedMolecule <= length to)
              return replacedMolecule

            nonSectionLength = (+ 1) . length . takeWhile (/= Ar) . reverse

            replacedMolecules :: [Molecule]
            replacedMolecules = do
              from <- froms
              (Rule element replacement) <- rules
              guard (element == last from)
              return $ initial from ++ replacement

    sectionMatch :: Molecule -> Section -> [MatchAttempt]
    sectionMatch from toSection = do
      fromSection <- relevantSections from
      packedSteps <- maybeToList $ countSteps (packed fromSection) (packed toSection)
      -- traceMAnnotated "matched a section pack with steps" packedSteps
      return MatchAttempt { steps = packedSteps
                          , fromExcess = before fromSection
                          , toExcess = before toSection }

      where
        relevantSections :: Molecule -> [Section]
        relevantSections [] = []
        relevantSections mol = map snd . filter ((last mol ==) . fst) $ sections

-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  input <- lines <$> getContents
  let rules = parseInput rulesParser . intercalate "\n" . takeWhile (/= "") $ input
  let molecule = parseInput moleculeParser . head . tail . dropWhile (/= "") $ input
  let sections = indexSections rules

  -- let test = "CRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFAr"
  -- print $ tailSection (parseInput moleculeParser test)
  print $ stepsToFullMatch rules sections [E] molecule

  where
    parseInput :: Parser a -> String -> a
    parseInput p = fromRight . parseOnly p . T.pack
    fromRight (Right b) = b
