{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Control.Monad.State
import Data.STRef
import Data.List
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

import Debug.Trace

with :: (b -> b -> c) -> (a -> b) -> (a -> a -> c) 
with f g a b = f (g a) (g b)

-- data Floor = Floor { items :: [Item] } deriving Show
type Floor = Set Item
data Item = Generator Tag
          | Microchip Tag
          deriving (Eq, Ord)
type Tag = String

data World = World { steps :: Int
                   , elevator :: Int
                   , floors :: Vector Floor
                   }

instance Show World where
  show w = "W" ++ show (steps w) ++ ": E" ++ show (elevator w) ++ " [" ++ showFloors ++ "]"
    where
      showFloors = intercalate "," . map showFloor . Vector.toList . floors $ w
      showFloor = show . sort . Set.toList

comparator = (,) <$> elevator <*> floors

instance Eq World where
  (==) = (==) `with` comparator

instance Ord World where
  compare = compare `with` comparator

small = map Set.fromList
  [ [Microchip "he", Microchip "li"]
  , [Generator "he"]
  , [Generator "li"]
  , []
  ]

building = map Set.fromList
  [ [Generator "promethium", Microchip "promethium"]
  , [Generator "cobalt", Generator "curium", Generator "ruthenium", Generator "plutonium"]
  , [Microchip "cobalt", Microchip "curium", Microchip "ruthenium", Microchip "plutonium"]
  , []
  ]

part2 = map Set.fromList
  [ [Generator "promethium", Microchip "promethium", Generator "elerium", Microchip "elerium", Generator "dilithium", Microchip "dilithium"]
  , [Generator "cobalt", Generator "curium", Generator "ruthenium", Generator "plutonium"]
  , [Microchip "cobalt", Microchip "curium", Microchip "ruthenium", Microchip "plutonium"]
  , []
  ]

instance Show Item where
  show (Generator x) = 'G' : take 2 x
  show (Microchip x) = 'M' : take 2 x

generators :: [Item] -> [Tag]
generators [] = []
generators (Generator x : xs) = x : generators xs
generators (_:xs) = generators xs

microchips :: [Item] -> [Tag]
microchips [] = []
microchips (Microchip x : xs) = x : microchips xs
microchips (_:xs) = microchips xs

-- parseFloor :: String -> Floor
-- parseFloor = fromJust . parseMaybe floorParser
--   where
--     word = (lexeme (spaceChar *> return ()) .)
--     floorParser :: Parser Floor
--     floorParser = do
--       word string "The"
--       number <- word (const floorNumberParser)
--       word string "contains"
--       items <- itemsParser
--       char '.'
-- 
--       return $ Floor number items
-- 
--     floorNumberParser :: Parser Int
--     floorNumberParser =
--       (string "first floor" *> 1)
--       <|> (string "second floor" *> 2)
--       <|> (string "third floor" *> 3)
--       <|> (string "fourth floor" *> 4)
-- 
--     itemsParser :: Parser [Item]
--     itemsParser =
--       (string "nothing relevant" *> return [])
--       <|> (sepBy itemParser (word string "and"))
--       <|> (undefined)
-- 
--     itemParser :: Parser Item
--     itemParser = undefined

indexSafeM :: (MonadPlus m) => Int -> Vector a -> m a
indexSafeM i v = maybe mzero return (v Vector.!? i)

bfs :: (Ord a, Show a) => (a -> [a]) -> [a] -> [a]
bfs = bfs' Set.empty
  where
    bfs' :: (Ord a, Show a) => Set a -> (a -> [a]) -> [a] -> [a]
    bfs' _ _ [] = []
    bfs' s getNeighbors nodes = trace debug $ nodes ++ traverse unique
      where
        debug = "n=" ++ show (length nodes) ++ "; s=" ++ show (Set.size s) ++ "; u=" ++ show (length unique) ++ "; s'=" ++ show (Set.size s)
        traverse = bfs' s' getNeighbors
        next = concatMap getNeighbors nodes
        -- unique = filter (`Set.notMember` s) next
        -- s' = foldr Set.insert s unique

        (s', unique) = foldr digest (s, []) next

        digest :: Ord a => a -> (Set a, [a]) -> (Set a, [a])
        digest x (s, xs)
          | Set.member x s = (s, xs)
          | otherwise = (Set.insert x s, x:xs)

--bfs :: (a -> [a]) -> [a] -> [a]
--bfs _ [] = []
--bfs getNeighbors nodes = nodes ++ traverse next
--  where
--    traverse = bfs getNeighbors
--    next = concatMap getNeighbors nodes

combinations :: Int -> [a] -> [[a]]
combinations max xs = do
  i <- [max,max-1..1]
  combinations' i xs

 where
   combinations' :: Int -> [a] -> [[a]]
   combinations' 0 _ = [[]]
   combinations' _ [] = []
   combinations' n (x:xs) =
     ((x:) <$> combinations' (n - 1) xs) ++ combinations' n xs

push = foldr Set.insert
pull = foldr Set.delete

moveItems :: [Item] -> Int -> Int -> Vector Floor -> Vector Floor
moveItems items from to = execState $ do
  floorFrom <- gets (Vector.! from)
  floorTo <- gets (Vector.! to)
  let updates = [(from, pull floorFrom items), (to, push floorTo items)]

  -- traceShowM ("move", items, from, to)
  -- traceShowM updates
  modify . flip Vector.update . Vector.fromList $ updates

-- Gp Mp ok
-- Mp ok
-- Gp ok
-- Gp Mp Mc bad
-- Mp Mc ok
-- Gp Mc bad
validFloorState :: Vector Floor -> Bool
validFloorState = Vector.all $ \(Set.toList -> items) ->
  null (generators items) || null (microchips items \\ generators items)

moves :: World -> [World]
moves w@World { elevator, floors, steps } = do
  -- traceM ("from: " ++ show w)
  dir <- [1, -1]
  let elevator' = elevator + dir
  guard (elevator' >= 0 && elevator' < length building)

  items <- Set.toList <$> indexSafeM elevator floors
  chosen <- combinations 2 items
  let floors' = moveItems chosen elevator elevator' floors
  guard (validFloorState floors')

  let world' = World { elevator = elevator', floors = floors', steps = steps + 1 }

  -- traceM ("to: " ++ show world')
  return world'

victory :: World -> Bool
victory World { elevator, floors } = elevator == 3 && all Set.null items
  where
    items :: [Set Item]
    items = (Vector.!) <$> [floors] <*> [0..2]

main = do
  -- input <- parse <$> getContents
  let input = building
  -- print (combinations 0 "abc")
  -- print (combinations 1 "abc")
  -- print (combinations 2 "abc")
  -- print (combinations 3 "abc")
  -- print (combinations 3 "abcd")

  -- print . solve1 $ small
  -- print . solve1 $ input
  print . solve1 $ part2




  --
  -- n * (n+1) / 2 = x
  -- n^2 + n - 2x = 0
  --
  -- -b +- sqrt(b^2 - 4ac) / 2a
  -- -1 +- sqrt(1 + 8x) / 2
  -- -1 + 3 / 2 = 1
  --
  --
  -- 1 -> 2 3
  -- 2 -> 4 5
  -- 4 -> 7 8
  -- 7 -> 11 12
  -- print . take 20 . bfs (\i -> [2 * i, 2 * i + 1]) $ [0]

 where
   solve1 = head . filter victory . (traceShow <$> length <*> id) . bfs moves . return . World 0 0 . Vector.fromList
   -- parse = map parseFloor . lines
