import Advent
import Control.Monad.State.Strict
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- set X Y sets register X to the value of Y.
-- sub X Y decreases register X by the value of Y.
-- mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
-- jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
data Instr = Set Reg Val
           | Sub Reg Val
           | Mul Reg Val
           | Jnz Val Int
           deriving Show

type Reg = Char

data Val = RegVal Char
         | ImmVal Int
         deriving Show

data S = S
  { pc :: !Int
  , regs :: !(Map Char Int)
  , muls :: !Int
  }

overPc f s = s { pc = f (pc s) }
overRegs f s = s { regs = f (regs s) }
overMuls f s = s { muls = f (muls s) }

instrParser :: Parser (Maybe Instr)
instrParser =
  (symbol "#" $> Nothing) <|>
  Just <$> choice
    [ (symbol "set" $> Set) <*> reg <*> val
    , (symbol "sub" $> Sub) <*> reg <*> val
    , (symbol "mul" $> Mul) <*> reg <*> val
    , (symbol "jnz" $> Jnz) <*> val <*> imm
    ]

 where
   imm :: Parser Int
   imm = L.lexeme space signedInt
   reg :: Parser Char
   reg = L.lexeme space $ satisfy (\c -> c >= 'a' && c <= 'h')
   val :: Parser Val
   val = (RegVal <$> reg) <|> (ImmVal <$> imm)

evalInstr :: Instr -> State S ()
evalInstr instr =
  case instr of
    Set reg val -> do
      v <- fromVal val
      modify $ overRegs (Map.insert reg v)
      next
    Sub reg val -> do
      v <- fromVal val
      updateReg reg (subtract v)
      next
    Mul reg val -> do
      modify $ overMuls (+ 1)
      v <- fromVal val
      updateReg reg (* v)
      next
    Jnz val offset -> do
      v <- fromVal val
      if v == 0
         then next
         else modify $ overPc (+ offset)
 where
   next = modify $ overPc (+ 1)

   updateReg :: Reg -> (Int -> Int) -> State S ()
   updateReg r f = modify $ overRegs (Map.alter (Just . f . fromMaybe 0) r)

   fromReg :: Reg -> State S Int
   fromReg r = gets (fromMaybe 0 . Map.lookup r . regs)

   fromVal :: Val -> State S Int
   fromVal (ImmVal val) = return val
   fromVal (RegVal reg) = fromReg reg

eval :: Vector Instr -> State S ()
eval prog = do
  instr <- gets ((prog Vector.!?) . pc)
  -- traceShowM instr
  case instr of
    Just instr -> do
      evalInstr instr
      eval prog
    Nothing -> return ()

initial :: Int -> S
initial a = S
  { pc = 0
  , regs = Map.fromList [('a', a)]
  , muls = 0 }

{-
b = 108400
c = 125400
main loop:
  f = 1
  d = 2
  inner loop:
    e = 2
    third loop:
      if (d * e == b):
        f = 0
      e += 1
      break if (e == b)
    d += 1
    break if (d == b)
  # at this point, e == d == b
  if f == 0:
    h += 1
  break if (b == c)
  b += 17
-}

reimplementation :: Int -> Int -> Int -> Int
reimplementation b c step = countBy isComposite [b, (b + step) .. c]
 where
   isComposite b = isJust $ find ((== 0) . (b `mod`)) [2 .. b - 1]

main :: IO ()
main = solve parse part1 part2
  where
    parse = Vector.fromList . mapMaybe (parseWith instrParser) . lines
    part1 = muls . (`execState` initial 0) . eval
    part2 _ = reimplementation 108400 125400 17
