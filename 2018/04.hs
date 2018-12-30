{-# LANGUAGE NamedFieldPuns #-}
import Advent
import Control.Arrow
import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Id = Int

data Action
  = BeginsShift
  | FallsAsleep
  | WakesUp
  deriving Show

type Date = String
type Hour = Int
type Minute = Int
type Time = (Date, Hour, Minute)

data Event = Event
  { time :: (Date, Hour, Minute)
  , guardId :: Id
  , action :: Action }
  deriving Show

type Schedule = [Event]

timeParser :: Parser Time
timeParser = do
  date <- someTill anyChar spaceChar
  hour <- decimal
  _ <- char ':'
  minute <- decimal
  return (date, hour, minute)

eventLineParser :: Parser (Time, Maybe Id, Action)
eventLineParser = do
  time <- between (char '[') (char ']') timeParser
  _ <- char ' '
  guardId <- optional (string "Guard #" *> decimal <* space)
  action <- choice
    [ string "begins shift" $> BeginsShift
    , string "falls asleep" $> FallsAsleep
    , string "wakes up" $> WakesUp
    ]
  return (time, guardId, action)

scheduleParser :: Parser Schedule
scheduleParser = do
  eventLines <- some (eventLineParser <* newline)
  sortBy (compare `on` view _1) eventLines
    & mapAccumL impliedGuard (First Nothing)
    & snd
    & return
 where
   impliedGuard lastGuard (time, maybeGuard, action) =
     let nextGuard = First maybeGuard <> lastGuard
         guardId = fromJust $ getFirst nextGuard
      in (nextGuard, Event { time, guardId, action })

toSleepSchedules :: Schedule -> Map Id [Minute]
toSleepSchedules = snd . foldl' f (0, mempty)
  where
    f :: (Minute, Map Id [Minute]) -> Event -> (Minute, Map Id [Minute])
    f (_, m) (Event _ _ BeginsShift) = (0, m)
    f (_, m) (Event (_, _, now) _ FallsAsleep) = (now, m)
    f (startedSleeping, m) (Event (_, _, now) guardId WakesUp) =
      let m' = foldl' (flip $ insertAppend guardId . return) m [startedSleeping .. now - 1]
       in (0, m')

findBest :: Map Id (Int, Minute) -> (Id, Minute)
findBest = snd . maximumBy (compare `on` fst) . map retuple . Map.toList
  where
    retuple (gid, (metric, minute)) = (metric, (gid, minute))

strategy1 :: Map Id [Minute] -> (Id, Minute)
strategy1 = findBest . fmap (length &&& mode)

strategy2 :: Map Id [Minute] -> (Id, Minute)
strategy2 = findBest . fmap ((uncurry count &&& fst) <<< (mode &&& id))

main :: IO ()
main = solve parse part1 part2
  where
    parse = sortBy (compare `on` time) . parseWith scheduleParser
    part1 = uncurry (*) . strategy1 . toSleepSchedules
    part2 = uncurry (*) . strategy2 . toSleepSchedules
