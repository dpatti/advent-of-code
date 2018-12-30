{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

import Advent
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Register = Int
type Value = Int
data Source = ValueSource Value | RegisterSource Register deriving Show
data Command = Cpy Source Source
             | Inc Register
             | Dec Register
             | Jnz Source Source
             | Tgl Register
             | Out Register
             deriving Show

data Cpu = Cpu { commands :: [Command]
               , pc :: Int
               , registers :: Map Register Value
               , out :: [Int]
               } deriving Show

parseCommand :: String -> Command
parseCommand = parseWith commandParser
  where
    commandParser :: Parser Command
    commandParser =
      (string "cpy " *> (Cpy <$> sourceParser <*> (char ' ' *> sourceParser)))
      <|> (string "inc " *> (Inc <$> registerParser))
      <|> (string "dec " *> (Dec <$> registerParser))
      <|> (string "jnz " *> (Jnz <$> sourceParser <*> (char ' ' *> sourceParser)))
      <|> (string "tgl " *> (Tgl <$> registerParser))
      <|> (string "out " *> (Out <$> registerParser))

    sourceParser =
      (ValueSource <$> valueParser)
      <|> (RegisterSource <$> registerParser)

    registerParser = ord <$> anyChar 
    valueParser = fromIntegral <$> signed (skipMany spaceChar) decimal

toggle :: Command -> Command
toggle (Inc r) = Dec r
toggle (Dec r) = Inc r
toggle (Jnz s i) = Cpy s i
toggle (Cpy s r) = Jnz s r
toggle (Tgl r) = Inc r

process :: Cpu -> Cpu
process cpu@Cpu { commands, pc, registers, out } =
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

    (Out (fromRegister -> value)) -> cpu { out = out ++ [value], pc = pc + 1 }

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
  solve1 input
  -- print . solve2 $ input

  where
    clock = 0 : 1 : clock
    parse = map parseCommand . lines
    init n commands = Cpu { pc = 0, registers = Map.singleton (ord 'a') n, commands = commands, out = [] }
    -- debug = map ((,) <$> pc <*> registers) . take 10 . takeWhile running . iterate process . init
    solve1 input =
      forM_ [1..] $ \i -> do
        let signal = out . fromJust . find ((> 10) . length . out) . iterate process . init i $ input

        if all (== True) (zipWith (==) signal clock)
           then print i
           else putStrLn $ "not " ++ show i

    -- solve1 = registers . last . takeWhile running . iterate process . init
    -- solve2 = registers . last . takeWhile running . iterate process . init2
    running Cpu { pc, commands } = pc < length commands

