{-# LANGUAGE ViewPatterns #-}

import Advent
import Control.Monad.State
import Data.Ord
import qualified Data.Map.Strict as Map

type Bot = Int
type Value = Int
data Sink = Bot Int | Output Int deriving (Eq, Ord, Show)
data Command = Init { bot :: Bot, value :: Value }
             | Distribute { source :: Bot, low :: Sink, high :: Sink }
             deriving Show
type Inventory = [Value]
type Factory = Map.Map Sink Inventory

traceFactory :: Factory -> Factory
traceFactory = trace <$> pretty <*> id
  where
    pretty = unlines . map prettyBot . sortBy (comparing fst) . Map.toList
    prettyBot (bot, inventory) = show bot ++ ": " ++ show inventory

parseCommand :: String -> Command
parseCommand = parseWith (initParser <|> distributeParser)
  where
    initParser :: Parser Command
    initParser = do
      string "value "
      value <- fromIntegral <$> decimal
      string " goes to bot "
      bot <- fromIntegral <$> decimal

      return $ Init bot value

    distributeParser :: Parser Command
    distributeParser = do
      string "bot "
      bot <- fromIntegral <$> decimal
      string " gives low to "
      low <- sinkParser
      string " and high to "
      high <- sinkParser

      return $ Distribute bot low high

    sinkParser :: Parser Sink
    sinkParser = do
      sinkType <- (string "bot" *> pure Bot) <|> (string "output" *> pure Output)
      char ' '
      id <- fromIntegral <$> decimal
      return (sinkType id)

addValue :: Sink -> Value -> Factory -> Factory
addValue sink value = Map.alter (Just . hold . fromMaybe []) sink
  where
    hold (a:b:_) = error $ show sink ++ " holding too many!"
    hold inv = value:inv

initializeState :: [Command] -> Factory
initializeState = foldr (uncurry addValue) Map.empty . mapMaybe toPair
  where
    toPair (Init bot value) = Just (Bot bot, value)
    toPair _ = Nothing

step :: [Command] -> Factory -> Factory
step commands factory = foldl' step' factory . filter isDistribute $ commands
  where
    isDistribute Distribute{} = True
    isDistribute _ = False

    step' :: Factory -> Command -> Factory
    step' factory' (Distribute (Bot -> source) low high)
      | factory /= factory' = factory'
      | otherwise = flip execState factory' $ do
          inv <- gets (fromMaybe [] . Map.lookup source)
          case inv of
            (sort -> [a, b]) -> do
              modify (addValue low a)
              modify (addValue high b)
              modify (Map.delete source)
            _ -> return ()

ensureUpdate :: Eq a => (a -> a) -> (a -> a)
ensureUpdate f x
  | x' == x = error "Stale!"
  | otherwise = x'
  where x' = f x

hasPair :: Int -> Int -> Factory -> Maybe Sink
hasPair a b = fmap fst . find pair . Map.toList
  where
    pair (bot, inv)
      | sort inv == sort [a, b] = True
      | otherwise = False

noBots :: Factory -> Bool
noBots = not . any isBot . Map.keys
  where
    isBot Bot{} = True
    isBot _ = False

getOutputs :: [Int] -> Factory -> [Value]
getOutputs (map Output -> os) factory =
  concat . mapMaybe (`Map.lookup` factory) $ os

main = do
  input <- parse <$> getContents
  -- print input
  print . solve1 $ input
  print . solve2 $ input

 where
   parse = map parseCommand . lines
   solve1 = head . mapMaybe (hasPair 17 61) . timeline
   solve2 = product . getOutputs [0, 1, 2] . head . filter noBots . timeline
   timeline = iterate <$> ensureUpdate . step <*> initializeState
