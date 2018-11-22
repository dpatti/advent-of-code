{-# LANGUAGE NamedFieldPuns #-}
import Advent
import Safe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Data = Zero | One deriving (Eq, Ord)

instance Show Data where
  show Zero = "0"
  show One = "1"

data Tape = Tape
  { cursor :: Data
  , left :: [Data]
  , right :: [Data]
  }

instance Show Tape where
  show Tape { cursor, left, right } =
    unwords $
      (show <$> reverse left)
      ++ [show cursor]
      ++ (show <$> right)

zeroTape :: Tape
zeroTape = Tape { cursor = Zero, left = [], right = [] }

write :: Data -> Tape -> Tape
write data_ tape = tape { cursor = data_ }

moveLeft :: Tape -> Tape
moveLeft Tape { cursor, left, right } =
  Tape
    { cursor = headDef Zero left
    , left = tailDef [] left
    , right = cursor : right }

moveRight :: Tape -> Tape
moveRight Tape { cursor, left, right } =
  Tape
    { cursor = headDef Zero right
    , left = cursor : left
    , right = tailDef [] right }

checksum :: Tape -> Int
checksum Tape { cursor, left, right } =
  count One $ cursor : left ++ right

type StateId = Char
type StateMachine = Map StateId Match
type Match = Map Data Action
data Action = Action
  { writer :: Tape -> Tape
  , mover :: Tape -> Tape
  , nextState :: StateId }

data S = S
  { state :: StateId
  , tape :: Tape }

step :: StateMachine -> S -> S
step sm S { state, tape } =
  case Map.lookup state sm >>= Map.lookup (cursor tape) of
    Nothing -> error $ "Missing state " ++ show state ++ " in state machine"
    Just Action { writer, mover, nextState } ->
      S { state = nextState
        , tape = mover . writer $ tape
        }

-- Yeah I'm not parsing this. Let's just use a DSL.
data Input = Input
  { initialState :: StateId
  , steps :: Int
  , stateMachine :: StateMachine }

type Branch = (Data, Tape -> Tape, StateId)

fromDsl :: [(StateId, Branch, Branch)] -> StateMachine
fromDsl dsl = Map.fromList states
  where
    states = do
      (state, zeroBranch, oneBranch) <- dsl
      let actions =
            [ (Zero, branch zeroBranch)
            , (One, branch oneBranch) ]
      return (state, Map.fromList actions)

    branch (toWrite, mover, nextState) =
      Action { writer = write toWrite, mover, nextState }

input :: Input
input = Input
  { initialState = 'A'
  , steps = 12994925
  , stateMachine = fromDsl
      [ ('A', (One, moveRight, 'B')
            , (Zero, moveLeft, 'F'))
      , ('B', (Zero, moveRight, 'C')
            , (Zero, moveRight, 'D'))
      , ('C', (One, moveLeft, 'D')
            , (One, moveRight, 'E'))
      , ('D', (Zero, moveLeft, 'E')
            , (Zero, moveLeft, 'D'))
      , ('E', (Zero, moveRight, 'A')
            , (One, moveRight, 'C'))
      , ('F', (One, moveLeft, 'A')
            , (One, moveRight, 'A'))
      ]
  }

main :: IO ()
main = solve parse part1 part2
  where
    parse = const input
    part1 =
      checksum . tape .
        (iterateN
          <$> steps
          <*> (step . stateMachine)
          <*> (flip S zeroTape . initialState))
    part2 = const "Yay!"
