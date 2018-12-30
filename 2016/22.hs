{-# LANGUAGE NamedFieldPuns #-}

import Advent hiding (distance)
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Node = Node { location :: Coord
                 , size :: Int
                 , used :: Int
                 }
                 deriving Show

data NodeStatus = Empty | Full | Static deriving Eq

instance Show NodeStatus where
  show Empty = "_"
  show Full = "."
  show Static = "#"

type Nodes = Map Coord NodeStatus
data Network = Network { nodes :: Nodes
                       , distance :: Coord -> Coord -> Int
                       }
type Moves = State Network Int

avail :: Node -> Int
avail = (-) <$> size <*> used

nodeAt :: Coord -> Network -> NodeStatus
nodeAt coord = (Map.! coord) . nodes

setNodes :: Nodes -> Network -> Network
setNodes nodes net = net { nodes }

updateNodes :: (Nodes -> Nodes) -> Network -> Network
updateNodes f = setNodes <$> (f . nodes) <*> id

parseLine :: String -> Node
parseLine = parseWith $ do
  string "/dev/grid/node-x"
  x <- fromIntegral <$> decimal
  string "-y"
  y <- fromIntegral <$> decimal
  many spaceChar
  size <- fromIntegral <$> decimal
  string "T"
  many spaceChar
  used <- fromIntegral <$> decimal

  return Node { location = Coord x y, size, used }

viable :: (Node, Node) -> Bool
viable (from, to)
  | used from <= 0 = False
  | location from == location to = False
  | used from > avail to = False
  | otherwise = True

mkNetwork :: [Node] -> Network
mkNetwork = calculatePaths . foldr insert Map.empty
  where
    insert Node { location, size, used } =
      Map.insert location (nodeStatus size used)
    nodeStatus size used
      | used > 80 = Static
      | used == 0 = Empty
      | otherwise = Full

calculatePaths :: Nodes -> Network
calculatePaths nodes = Network { nodes, distance }
  where
    distance = undefined

keyFor :: Eq a => a -> Map k a -> Maybe k
keyFor value = Map.foldrWithKey findValue Nothing
  where
    findValue _ _ (Just k) = Just k
    findValue k v Nothing
      | value == v = Just k
      | otherwise = Nothing

moveSpaceTo :: Coord -> Moves
moveSpaceTo target = do
  spacePos <- gets $ fromJust . keyFor Empty . nodes
  dist <- gets $ ($ target) . ($ spacePos) . distance
  swap target spacePos
  return dist

swap :: Coord -> Coord -> Moves
swap a b = do
  aStatus <- gets $ (Map.! a) . nodes
  bStatus <- gets $ (Map.! b) . nodes
  modify $ updateNodes (Map.insert a bStatus)
  modify $ updateNodes (Map.insert b aStatus)
  return 1

accessData :: Int -> Moves
accessData 0 = return 0
accessData y = fmap sum . sequence $
  [ moveSpaceTo (Coord 0 (y - 1))
  , swap (Coord 0 (y - 1)) (Coord 0 y)
  , accessData (y - 1)
  ]

pretty :: Nodes -> String
pretty ns = do
  y <- [0..30]
  concatMap (show . (ns Map.!) . (`Coord` y)) [0..33] ++ "\n"

main = do
  input <- parse <$> getContents
  -- print input
  -- print . solve1 $ input
  -- putStrLn . debug $ input
  -- print . sort . map used $ input
  -- print . sort . map size $ input
  print $ 16 + 25 + 32 + (5 * 32) + 1

 where
   parse = map parseLine . drop 2 . lines
   solve1 = length . filter viable . allPairs
   -- part 2 is maybe as follows:
   --   the goal is at (0, 30)
   --   find the shortest path to get the space to (0, 29)
   --   spend one move to swap the two
   --   find the shortest path to get the space to (0, 28)
   --   repeat
   debug = pretty . nodes . mkNetwork
   -- solve2 = evalState (accessData 30) . mkNetwork
