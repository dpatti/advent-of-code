{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

import Advent

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Safe

type Reg = Char
data Val = Lit Int | Reg Reg deriving Show

data Inst = Snd Val
          | Set Reg Val
          | Add Reg Val
          | Mul Reg Val
          | Mod Reg Val
          | Rcv Reg
          | Jgz Val Val
          deriving Show

data Memory = Memory { pc :: Int -- program counter (index of next instruction)
                     , rc :: Int -- run counter (total instructions executed)
                     , lastFreq :: Maybe Int
                     , regs :: !(Map Char Int)
                     , pid :: Int }
  deriving Show

type Computation = ReaderT [Int] (WriterT [Int] (State Memory))

evalComputation :: Int -> [Int] -> Computation () -> [Int]
evalComputation pid input =
  (`evalState` initialState) . execWriterT . (`runReaderT` input)
 where
   initialState = Memory { pc=0, rc=0, lastFreq=empty, regs=mempty, pid }

instParser :: Parser Inst
instParser =
  choice
    [ (string "snd " $> Snd) <*> val
    , (string "set " $> Set) <*> reg <* space <*> val
    , (string "add " $> Add) <*> reg <* space <*> val
    , (string "mul " $> Mul) <*> reg <* space <*> val
    , (string "mod " $> Mod) <*> reg <* space <*> val
    , (string "rcv " $> Rcv) <*> reg
    , (string "jgz " $> Jgz) <*> val <* space <*> val
    ]

 where
   reg = satisfy (`elem` ['a'..'z'])
   val = (Lit <$> signed space decimal) <|> (Reg <$> reg)

valueOf :: Val -> Computation Int
valueOf (Lit val) = return val
valueOf (Reg reg) = gets (Map.findWithDefault 0 reg . regs)

-- outputs a list of all received frequencies
loadProgram :: Vector Inst -> Computation () -> Computation ()
loadProgram instructions step = do
  pc <- gets pc
  case instructions !? pc of
    Nothing -> return ()
    Just (Set reg val) -> update reg val (flip const)
    Just (Add reg val) -> update reg val (+)
    Just (Mul reg val) -> update reg val (*)
    Just (Mod reg val) -> update reg val mod
    Just (Snd val) -> do
      freq <- valueOf val
      modify (\s -> s { lastFreq = Just freq })
      continue
    Just (Rcv reg) -> do
      nonzero <- (/= 0) <$> valueOf (Reg reg)
      branch nonzero $ do
        freq <- fromJust <$> gets lastFreq
        tell [freq]
        update reg (Lit freq) (flip const)
    Just (Jgz test val) -> do
      offset <- valueOf val
      isPositive <- (> 0) <$> valueOf test
      branch isPositive $
        modify (\s -> s { pc = pc + offset }) >> step

 where
    branch b m = if b then m else continue

    continue = modify (\s -> s { pc = pc s + 1 }) >> step

    update reg val f = do
      rhs <- valueOf val
      modify $ \s -> s { regs = regs s & upsert (force . (`f` rhs) . fromMaybe 0) reg }
      continue

    force a = seq a a

patchedProgram :: Vector Inst -> Computation () -> Computation ()
patchedProgram instructions step = do
  pc <- gets pc
  rc <- gets rc
  modify $ \s -> s { rc = rc + 1 }
  case instructions !? pc of
    Just (Snd val) -> do
      output <- valueOf val
      tell [output]
      modify $ \s -> s { pc = pc + 1 }
      step
    Just (Rcv reg) -> do
      input <- asks head
      regs <- upsert (const input) reg <$> gets regs
      modify $ \s -> s { pc = pc + 1, regs }
      local tail step
    _ -> unpatchedProgram step
 where
   unpatchedProgram = loadProgram instructions

-- This uses mutually recursive values, and in the case of a deadlock, the
-- program is supposed to terminated. In my case, the GHC runtime detects that
-- and actually prints <<loop>>. I thought this was pretty cool, so I just had
-- it count out messages and eventually it will die. The only catch is that you
-- have to run it in interpreter mode because the compiled code won't print all
-- the numbers before it stops.
spawnPair :: Computation () -> ([Int], [Int])
spawnPair comp =
  let a = evalComputation 0 b (withPid 0 >> comp)
      b = evalComputation 1 a (withPid 1 >> comp)
   in (a, b)

 where
   withPid p = modify $ \s -> s { regs = regs s & Map.insert 'p' p }

main :: IO ()
main = solve parse part1 part2
  where
    parse = map (parseWith instParser) . lines
    part1 = headMay . evalComputation 0 [] . fix . loadProgram . Vector.fromList
    part2 = zip [1..] . snd . spawnPair . fix . patchedProgram . Vector.fromList
