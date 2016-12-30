{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace

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

-- Returns the tail length of a molecule up to the next section
nonSectionLength :: Molecule -> Int
-- nonSectionLength = (+ 1) . length . takeWhile (/= Ar) . reverse
nonSectionLength = length

-- -----------------------------------------------------------------------------
-- match logic

data MatchAttempt = MatchAttempt { steps :: Int
                                 , fromExcess :: Molecule
                                 , toExcess :: Molecule }
                                 deriving Show

data Key = Key { keyFrom :: Molecule
               , keyTo :: Molecule }
               deriving (Eq, Ord, Show)
type Memo = Map.Map Key [Int]

exactMatch :: MatchAttempt
exactMatch = MatchAttempt { steps = 0
                          , fromExcess = empty
                          , toExcess = empty }

-- really should use a reader here
stepsToFullMatch :: [Rule] -> SectionIndex -> Molecule -> Molecule -> Maybe Int
stepsToFullMatch rules sections = (listToMaybe <$>) . (withMemo <$>) . countSteps
  where
    withMemo :: State Memo a -> a
    withMemo = (`evalState` Map.empty)

    -- something that might be very wrong with countSteps: even though tryMatch
    -- is built to return the match with the fewest number of steps first, we do
    -- a depth-first search on that, which might mean the first is not the best.
    -- we almost definitely need a priority queue?
    countSteps :: Molecule -> Molecule -> State Memo [Int]
    countSteps [] [] = return (return 0)
    countSteps _ [] = return empty
    countSteps [] _ = return empty
    countSteps from to = memoize $ do
      traceM $ "matching from " ++ show from ++ " to " ++ show to
      triedMatches <- tryMatch from to
      (concat <$>) . forM triedMatches $ \m@(MatchAttempt steps from' to') ->
        map (steps +) <$> countSteps from' to'

        where
          key :: Key
          key = Key { keyFrom = from
                    , keyTo = to }

          memoize :: State Memo [Int] -> State Memo [Int]
          memoize eval = do
            memo <- get
            case Map.lookup key memo of
              (Just cached) -> do
                traceM $ "using cached of " ++ show key
                return cached
              Nothing -> do
                cached <- eval
                put $ Map.insert key cached memo
                return cached

    -- also i think we want a stack of ListT and StateT for the match attempt
    tryMatch :: Molecule -> Molecule -> State Memo [MatchAttempt]
    tryMatch from to = 
      -- See if we can section match or have to brute force
      case tailSection to of
        Nothing -> return (bruteMatch from to)
        (Just toSection) -> sectionMatch from toSection

    bruteMatch :: Molecule -> Molecule -> [MatchAttempt]
    bruteMatch from to = bruteMatch' [from] 0

      where
        bruteMatch' :: [Molecule] -> Int -> [MatchAttempt]
        bruteMatch' [] _
          | null to = return exactMatch
          | otherwise = empty
        bruteMatch' froms steps = theseMatches ++ moreMatches
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
              where
                nextMolecules = do
                  replacedMolecule <- replacedMolecules
                  guard (length replacedMolecule <= nonSectionLength to)
                  return replacedMolecule

            replacedMolecules :: [Molecule]
            replacedMolecules = do
              from <- froms
              (Rule element replacement) <- rules
              guard (element == last from)
              return $ initial from ++ replacement

    sectionMatch :: Molecule -> Section -> State Memo [MatchAttempt]
    sectionMatch from toSection =
      (concat <$>) . forM (relevantSections from) $ \fromSection -> do
        packedSteps <- countSteps (packed fromSection) (packed toSection)
        return $ do
          packedStepsOne <- packedSteps
          -- traceMAnnotated "matched a section pack with steps" packedSteps
          return MatchAttempt { steps = packedStepsOne
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
