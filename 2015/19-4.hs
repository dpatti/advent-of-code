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

-- a reverse list type
data ReverseList a = RNil | RCons (ReverseList a) a
  deriving Show

instance Functor ReverseList where
  fmap f RNil = RNil
  fmap f (RCons xs x) = RCons (fmap f xs) (f x)

instance Semigroup (ReverseList a) where
  RNil <> rlb = rlb
  rla <> RNil = rla
  rla <> (RCons xs x) = RCons (rla `mappend` xs) x

instance Monoid (ReverseList a) where
  mempty = RNil

instance Monad ReverseList where
  return = RCons RNil
  RNil >>= f = RNil
  (RCons xs x) >>= f = mappend (xs >>= f) (f x)

instance Applicative ReverseList where
  pure = return
  rlf <*> rlx = do
    f <- rlf
    x <- rlx
    return (f x)

listToReverse :: [a] -> ReverseList a
listToReverse = foldl' RCons RNil

rLast :: ReverseList a -> Maybe a
rLast RNil = Nothing
rLast (RCons _ x) = Just x

rInitial :: ReverseList a -> ReverseList a
rInitial RNil = RNil
rInitial (RCons xs x) = xs

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

data Element = E | Al | B | Ca | F | H | Mg | N | O | P | Si | Th | Ti 
             | Ar | C | Rn | Y
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

isTerminal :: MatchAttempt -> Bool
isTerminal = (||) <$> (null . fromExcess) <*> (null . toExcess)

priority :: MatchAttempt -> [Int]
priority (MatchAttempt steps fromExcess toExcess) = [outrageous, remainingSteps, excess]
  where 
    outrageous = length fromExcess `div` 4
    remainingSteps = steps + length toExcess `div` 2
    excess = length toExcess ^ 2 - length fromExcess ^ 2

instance Ord MatchAttempt where
  compare a b = compare (priority a) (priority b)

type MatchQueue = MinQueue.MinQueue MatchAttempt

exactMatch :: Int -> MatchAttempt
exactMatch steps = MatchAttempt { steps = steps
                                , fromExcess = empty
                                , toExcess = empty }

-- really should use a reader here
stepsToFullMatch :: [Rule] -> SectionIndex -> Molecule -> Molecule -> Maybe Int
stepsToFullMatch rules sections from to = countSteps (MatchAttempt 0 from to)

  where
    countSteps :: MatchAttempt -> Maybe Int
    countSteps attempt = do
      -- traceM $ "matching " ++ show attempt
      listToMaybe $ do
        -- traceM $ "before: " ++ show attempt
        -- let attempt' = yReduction attempt
        -- traceM $ "after: " ++ show attempt'
        matchAttempt <- exploreMatches (MinQueue.singleton attempt)
        traceM $ "checking match " ++ show matchAttempt
        case matchAttempt of
          (MatchAttempt steps [] []) -> return steps
          _ -> empty

      where
        exploreMatches :: MatchQueue -> [MatchAttempt]
        exploreMatches queue =
          case MinQueue.minView queue of
            Nothing -> []
            Just (attempt, queue') ->
              attempt : exploreMatches (nextAttemptsFrom attempt queue')

        nextAttemptsFrom :: MatchAttempt -> MatchQueue -> MatchQueue
        nextAttemptsFrom attempt queue = foldr enqueue queue $ do
          step <- stepMatch attempt
          guard (length (fromExcess step) <= length (toExcess step))
          return step

        enqueue :: MatchAttempt -> MatchQueue -> MatchQueue
        enqueue attempt queue =
          -- trace ("inserting " ++ show attempt ++ " into queue") $
            MinQueue.insert attempt queue

    {-
    just do it by hand ya dummy
    yReduction :: MatchAttempt -> MatchAttempt
    yReduction (MatchAttempt steps from to) = MatchAttempt steps' from to'
      where
        (steps', to') = yReduction' 0 to

        appendReduction :: (Int, Molecule) -> (Int, Molecule) -> (Int, Molecule)
        appendReduction a b = (fst a + fst b, x)
          where x = snd a <> snd b

        yReduction' :: Int -> Molecule -> (Int, Molecule)
        yReduction' s [] = (s, [])
        yReduction' steps to =
          trace ("yReduction' " ++ show (steps, to)) $

          case tailSection to of
            Nothing -> skipToNext
            (Just toSection) -> 
              if isMinimalYSection toSection
                 then yReduction' steps (before toSection)
                       `appendReduction` yReduce toSection
                 else skipToNext
          where
            skipToNext = yReduction' steps (initial to)
                         `appendReduction` (0, [last to])
            isMinimalYSection (Section before packed) =
              Y `elem` packed && Rn `notElem` packed

            yReduce :: Section -> (Int, Molecule)
            yReduce (Section before packed) =
              foldr appendReduction (0, [])
                [(0, [Rn]), toF preY, (0, [Y]), toF postY, (0, [Ar])]

              where
                preY = takeWhile (/= Y) packed
                postY = tail (dropWhile (/= Y) packed)

                toF :: Molecule -> (Int, Molecule)
                toF mol = (fromJust (countSteps (MatchAttempt 0 [F] mol)), [F])
    -}


    stepMatch :: MatchAttempt -> [MatchAttempt]
    stepMatch attempt@(MatchAttempt steps from to)
      -- traceM $ "stepMatch for " ++ show attempt
      | isTerminal attempt = empty
      | otherwise = reduceMatch attempt
                <|> sectionMatch attempt
                <|> bruteMatch attempt

    reduceMatch :: MatchAttempt -> [MatchAttempt]
    reduceMatch attempt@(MatchAttempt _ from to)
      | last from == last to =
        return attempt { fromExcess = initial from, toExcess = initial to }
      | otherwise = empty

    sectionMatch :: MatchAttempt -> [MatchAttempt]
    sectionMatch (MatchAttempt steps from to) =
      case tailSection to of
        Nothing -> empty
        (Just toSection) -> do
          fromSection <- relevantSections from
          guard (length (packed fromSection) <= length (packed toSection))
          packedSteps <- maybeToList $
            countSteps (MatchAttempt 0 (packed fromSection) (packed toSection))
          -- traceMAnnotated "matched a section pack with steps" packedSteps
          return MatchAttempt { steps = steps + packedSteps + 1
                              , fromExcess = before fromSection
                              , toExcess = before toSection }

      where
        relevantSections :: Molecule -> [Section]
        relevantSections [] = []
        relevantSections mol = map snd . filter ((last mol ==) . fst) $ sections

    bruteMatch :: MatchAttempt -> [MatchAttempt]
    bruteMatch attempt@(MatchAttempt steps from to) = do
      (Rule element replacement) <- rules
      guard (Ar /= last from)
      guard (element == last from)
      -- guard (length replacedFrom <= length to)
      return attempt { steps = steps + 1
                     , fromExcess = initial from ++ replacement }

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
