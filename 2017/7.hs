{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

import Advent

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

data Section =
  Section { name :: String
          , weight :: Int
          , programs :: [String]
          } deriving Show

parseLine :: String -> Section
parseLine = readParts . concatMap (splitOn " ") . splitOn ", "
  where
    readParts [name, read -> weight] = Section { name, weight, programs = [] }
    readParts (name : (read -> weight) : "->" : programs) =
      Section { name, weight, programs }

findRoot :: [Section] -> String
findRoot tree = head $ nodes \\ dependents
  where
    dependents = concatMap programs tree
    nodes = map name tree

index :: [Section] -> Map String Section
index = Map.fromList . map (\x -> (name x, x))

data Tree a =
  Tree { node :: a
       , children :: [Tree a]
       } deriving Show

instance Functor Tree where
  fmap f Tree { node, children } =
    Tree { node = f node, children = map (fmap f) children }

sectionTree :: [Section] -> Tree Section
sectionTree sections = loop . findRoot $ sections
  where
    indexed :: Map String Section
    indexed = index sections

    lookupSection :: String -> Section
    lookupSection name = fromJust $ Map.lookup name indexed

    loop :: String -> Tree Section
    loop (lookupSection -> node) =
      Tree { node, children = map loop (programs node) }

withTotalWeight :: Tree Section -> Tree (Section, Int)
withTotalWeight tree = Tree { node = node', children = children' }
 where
   node' = (node tree, total)
   children' = map withTotalWeight (children tree)
   total = sum (map (snd . node) children') + (weight . node $ tree)

corrections :: [Int] -> [Int]
corrections [a, b] = [b - a, a - b]
corrections xs = map (subtract (mode xs)) xs

findUnbalanced :: [Section] -> Int
findUnbalanced listed = fromJust $ loop inbalance root
  where
    root :: Tree (Section, Int)
    root = withTotalWeight (sectionTree listed)

    deltas = corrections . map (snd . node) . children

    findOne :: (a -> Bool) -> [a] -> a
    findOne f xs =
      case filter f xs of
        [x] -> x
        _ -> error $ "Expected exactly one result, got " ++ show (length xs)

    inbalance = findOne (/= 0) (deltas root)

    loop :: Int -> Tree (Section, Int) -> Maybe Int
    loop 0 _ = Nothing
    loop delta tree =
      -- Either we apply the delta to our node weight, or we apply it to one
      -- descendent
      let ds = deltas tree
          descendents = catMaybes (zipWith loop ds (children tree))
       in case descendents of
            [] -> Just $ (weight . fst . node $ tree) - delta
            [x] -> Just x
            xs -> error $ "Multiple results: " ++ show xs

main :: IO ()
main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

 where
   parse = map parseLine . lines
   solve1 = findRoot
   solve2 = findUnbalanced
