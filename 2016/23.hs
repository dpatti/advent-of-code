{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

import Control.Applicative
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

import Debug.Trace

type Register = Int
type Value = Int
data Source = ValueSource Value | RegisterSource Register deriving Show
data Command = Cpy Source Source
             | Inc Register
             | Dec Register
             | Jnz Source Source
             | Tgl Register
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
      (string "cpy " *> (Cpy <$> sourceParser <*> (char ' ' *> sourceParser)))
      <|> (string "inc " *> (Inc <$> registerParser))
      <|> (string "dec " *> (Dec <$> registerParser))
      <|> (string "jnz " *> (Jnz <$> sourceParser <*> (char ' ' *> sourceParser)))
      <|> (string "tgl " *> (Tgl <$> registerParser))

    sourceParser =
      (ValueSource <$> valueParser)
      <|> (RegisterSource <$> registerParser)

    registerParser = ord <$> anyChar 
    valueParser = fromIntegral <$> signed (skipMany spaceChar) decimal

upsert :: Ord k => (Maybe a -> a) -> k -> Map k a -> Map k a
upsert f = Map.alter (Just . f)

toggle :: Command -> Command
toggle (Inc r) = Dec r
toggle (Dec r) = Inc r
toggle (Jnz s i) = Cpy s i
toggle (Cpy s r) = Jnz s r
toggle (Tgl r) = Inc r

process :: Cpu -> Cpu
process cpu@Cpu { commands, pc, registers } =
  case command of
    (Inc register) ->
      cpu { pc = pc + 1, registers = upsert (succ . fromMaybe 0) register registers }
    (Dec register) ->
      cpu { pc = pc + 1, registers = upsert (pred . fromMaybe 0) register registers }
    (Cpy source dest) ->
      case dest of
        (ValueSource _) -> cpu { pc = pc + 1 }
        (RegisterSource register) ->
          cpu { pc = pc + 1, registers = Map.insert register (fromSource source) registers }
    (Jnz source (fromSource -> offset))
      | fromSource source == 0 -> cpu { pc = pc + 1 }
      | otherwise -> cpu { pc = pc + offset }
    (Tgl (traceShowId . fromRegister -> offset))
      | pc + offset < 0 || pc + offset >= length commands -> cpu { pc = pc + 1 }
      | otherwise ->
          let amt = pc + offset
              target = traceShowId (commands !! amt)
           in cpu { pc = pc + 1
                  , commands = take amt commands ++ [toggle target] ++ drop (amt + 1) commands
                  }

  where
    command = commands !! pc
    fromSource source =
      case source of
        (ValueSource value) -> value
        (RegisterSource register) -> fromRegister register

    fromRegister register = fromMaybe 0 . Map.lookup register $ registers

main = do
  input <- parse <$> getContents
  -- print input
  print . solve1 $ input
  -- print . solve2 $ input
  print (product [1..12] + 94 * 99)

  where
    parse = map parseCommand . lines
    init commands = Cpu { pc = 0, registers = Map.singleton (ord 'a') 7, commands = commands }
    init2 commands = Cpu { pc = 0, registers = Map.singleton (ord 'a') 12, commands = commands }
    debug = map ((,) <$> pc <*> registers) . take 10 . takeWhile running . iterate process . init
    solve1 = registers . last . takeWhile running . iterate process . init
    solve2 = registers . last . takeWhile running . iterate process . init2
    running Cpu { pc, commands } = pc < length commands
