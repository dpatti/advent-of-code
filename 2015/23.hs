{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Maybe
import Data.Text
import Data.Attoparsec.Text
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map

type Register = Char
type Offset = Int

type Registers = Map.Map Register Int

data State = State
  { registers :: Registers
  , pc :: Int
  } deriving Show

data Instruction = Half Register
                 | Triple Register
                 | Increment Register
                 | Jump Offset
                 | JumpIfEven Register Offset
                 | JumpIfOne Register Offset
                 deriving Show

type Program = Vector.Vector Instruction

instructionsParser :: Parser [Instruction]
instructionsParser = many (instruction <* endOfLine)
  where
    instruction = halfParser
              <|> tripleParser
              <|> incrementParser
              <|> jumpParser
              <|> jumpIfEvenParser
              <|> jumpIfOneParser

    register = anyChar
    offset = signed decimal

    halfParser = string "hlf " *> (Half <$> register)
    tripleParser = string "tpl " *> (Triple <$> register)
    incrementParser = string "inc " *> (Increment <$> register)
    jumpParser = string "jmp " *> (Jump <$> offset)
    jumpIfEvenParser = string "jie " *> (JumpIfEven <$> register <*> (string ", " *> offset))
    jumpIfOneParser = string "jio " *> (JumpIfOne <$> register <*> (string ", " *> offset))

evaluate :: Program -> State
evaluate prog = evaluate' initialState 
  where
    initialState :: State
    initialState = State { registers = Map.fromList [('a', 1)], pc = 0 }

    evaluate' :: State -> State
    evaluate' state = loop . perform (currentInstruction state) $ state

    loop :: State -> State
    loop state
      | pc state >= Vector.length prog = state
      | otherwise = evaluate' state

    currentInstruction :: State -> Instruction
    currentInstruction state = prog Vector.! pc state

    perform :: Instruction -> State -> State
    perform (Half r) state = step . update (`div` 2) r $ state
    perform (Triple r) state = step . update (* 3) r $ state
    perform (Increment r) state = step . update (+ 1) r $ state
    perform (Jump o) state = jump o state
    perform (JumpIfEven r o) state
      | get r state `mod` 2 == 0 = jump o state
      | otherwise = step state
    perform (JumpIfOne r o) state
      | get r state == 1 = jump o state
      | otherwise = step state
    
    jump :: Int -> State -> State
    jump offset state = 
      state { pc = pc state + offset }

    step = jump 1

    get :: Register -> State -> Int
    get r state = Map.findWithDefault 0 r (registers state)

    update :: (Int -> Int) -> Register -> State -> State
    update f r state =
      state { registers = Map.alter (Just . f . fromMaybe 0) r (registers state) }


main :: IO ()
main = do
  input <- pack <$> readFile "23.input"
  print . evaluate . Vector.fromList . fromRight . parseOnly instructionsParser $ input

 where fromRight (Right x) = x
