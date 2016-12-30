{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

type Register = Int
type Value = Int
data Source = ValueSource Value | RegisterSource Register deriving Show
data Command = Cpy Source Register
             | Inc Register
             | Dec Register
             | Jnz Source Int
             deriving Show

data Cpu = Cpu { commands :: [Command]
               , pc :: Int
               , registers :: Map Register Value
               } deriving Show

fromRight (Left e) = error (show e)
fromRight (Right v) = v

parseCommand :: String -> Command
parseCommand = fromRight . parse commandParser "test"
  where
    commandParser :: Parser Command
    commandParser =
      (string "cpy " *> (Cpy <$> sourceParser <*> (char ' ' *> registerParser)))
      <|> (string "inc " *> (Inc <$> registerParser))
      <|> (string "dec " *> (Dec <$> registerParser))
      <|> (string "jnz " *> (Jnz <$> sourceParser <*> (char ' ' *> valueParser)))

    sourceParser =
      (ValueSource <$> valueParser)
      <|> (RegisterSource <$> registerParser)

    registerParser = ord <$> anyChar 
    valueParser = fromIntegral <$> signed (skipMany spaceChar) decimal

upsert :: Ord k => (Maybe a -> a) -> k -> Map k a -> Map k a
upsert f = Map.alter (Just . f)

process :: Cpu -> Cpu
process cpu@Cpu { commands, pc, registers } =
  case command of
    (Inc register) ->
      cpu { pc = pc + 1, registers = upsert (succ . fromMaybe 0) register registers }
    (Dec register) ->
      cpu { pc = pc + 1, registers = upsert (pred . fromMaybe 0) register registers }
    (Cpy source register) ->
      cpu { pc = pc + 1, registers = Map.insert register (fromSource source) registers }
    (Jnz source offset)
      | fromSource source == 0 -> cpu { pc = pc + 1 }
      | otherwise -> cpu { pc = pc + offset }

  where
    command = commands !! pc
    fromSource source =
      case source of
        (ValueSource value) -> value
        (RegisterSource register) -> fromMaybe 0 . Map.lookup register $ registers

main = do
  input <- parse <$> getContents
  -- print input
  print . solve1 $ input
  print . solve2 $ input

  where
    parse = map parseCommand . lines
    init commands = Cpu { pc = 0, registers = Map.empty, commands = commands }
    init2 commands = Cpu { pc = 0, registers = Map.singleton (ord 'c') 1, commands = commands }
    debug = map ((,) <$> pc <*> registers) . take 10 . takeWhile running . iterate process . init
    solve1 = registers . last . takeWhile running . iterate process . init
    solve2 = registers . last . takeWhile running . iterate process . init2
    running Cpu { pc, commands } = pc < length commands
