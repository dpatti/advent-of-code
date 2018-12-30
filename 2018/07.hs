{-# LANGUAGE LambdaCase, TemplateHaskell #-}
import Advent

import Control.Lens
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Set (Set)
import qualified Data.Set as Set

type Step = Char
type Dependencies = Map Step [Step]

parseDependency :: Parser (Step, Step)
parseDependency = do
  _ <- string "Step "
  from <- anyChar
  _ <- string " must be finished before step "
  to <- anyChar
  _ <- string " can begin."
  return (to, from)

-- Oh, I misread
topoSort :: Dependencies -> [Step] -> [[Step]]
topoSort _ [] = []
topoSort deps targets =
  let (fulfilled, blocked) = partition isReady targets
   in sort fulfilled : topoSort (Map.mapMaybe (remove fulfilled) deps) blocked

 where
   isReady target = not (Map.member target deps)
   remove fulfilled xs =
     case xs \\ fulfilled of
       [] -> Nothing
       xs' -> Just xs'

drawDeps :: Dependencies -> [Step] -> String
drawDeps deps steps = ("Dependencies:\n" ++) . intercalate "\n" $ map toLine steps
  where
    toLine step =
      case Map.lookup step deps of
        Nothing -> step : " -> _"
        Just xs -> step : " -> " ++ show xs

resolve :: Dependencies -> [Step] -> [Step]
resolve _ [] = []
resolve deps steps = -- trace (drawDeps deps steps) $
  let next = fromJust . find (`Map.notMember` deps) $ steps
   in next : resolve (Map.mapMaybe (remove next) deps) (steps \\ [next])
 where
  remove x xs =
    case xs \\ [x] of
      [] -> Nothing
      xs' -> Just xs'

toSteps :: Dependencies -> [Step]
toSteps deps =
  sortNub (Map.keys deps ++ concat (Map.elems deps))

data S = S
  { _workers :: MinQueue (Int, Step)
  , _remaining :: Set Step
  , _elapsed :: Int
  }
makeLenses ''S

inProgress :: Fold S (Set Step)
inProgress = workers . folded . _2 . to Set.singleton

isReady :: Dependencies -> Step -> State S Bool
isReady deps step = do
  stepsLeft <- use remaining
  return . Set.null . Set.intersection stepsLeft . Set.fromList . Map.findWithDefault [] step $ deps

instance Foldable MinQueue where
  foldr = MinQueue.foldrU

-- Let's write some incredibly imperative looking code in a purely functional
-- language. Because lenses.
tick :: Int -> Dependencies -> State S (Maybe Int)
tick capacity deps = do
  -- Assign new workers
  stepsLeft <- liftA2 (Set.\\) (use remaining) (use inProgress)
  next <- map (cost &&& id) <$> filterM (isReady deps) (Set.toList stepsLeft)
  available <- (capacity -) <$> uses workers MinQueue.size
  forM_ (take available next) $ \entry ->
    workers %= MinQueue.insert entry
  -- Pop the next to finish
  (time, step) <- workers %%= MinQueue.deleteFindMin
  elapsed += time
  remaining %= Set.delete step
  workers %= MinQueue.mapU (_1 -~ time)
  -- Check if we're done
  isDone <- uses remaining Set.null
  if isDone
    then Just <$> use elapsed
    else return Nothing

cost :: Step -> Int
cost step = 60 + ord step - ord 'A' + 1

resolveParallel :: Int -> Dependencies -> [Step] -> Int
resolveParallel n deps steps = findState initial (tick n deps)
 where
   initial :: S
   initial = S
     { _workers = MinQueue.empty
     , _remaining = Set.fromList steps
     , _elapsed = 0 }

main :: IO ()
main = solve parse part1 part2
  where
    parse = fromListMulti . map (parseWith parseDependency) . lines
    part1 = resolve <$> id <*> toSteps
    part2 = resolveParallel 5 <$> id <*> toSteps
