import Control.Applicative
import Control.Monad
import qualified Data.Set as Set
import Data.List
import Data.Function
import Debug.Trace

type Weight = Int
type Weights = [Weight]
-- type WeightSet = [Weights]
type Score = (Int, Int)

-- data Cluster = Balanced WeightSet | Unbalanced

-- partitions :: Weights -> Weight -> [WeightSet]
-- partitions presents size = do
--   clustering <- map cluster (permutations presents)
--   case clustering of 
--     Unbalanced -> empty
--     (Balanced ws) -> return ws
-- 
--   where
--     cluster :: Weights -> Cluster
--     cluster = foldr cluster' (Balanced (pure empty))
-- 
--     cluster' :: Weight -> Cluster -> Cluster
--     cluster' _ Unbalanced = Unbalanced
--     cluster' w (Balanced set@(group : rest)) =
--       case sum group `compare` size of
--         LT -> Balanced ((w : group) : rest)
--         EQ -> Balanced ([w] : set)
--         GT -> Unbalanced
-- 
-- equalGroups :: Weights -> Int -> [WeightSet]
-- equalGroups presents count = do
--   group <- partitions presents size
--   guard (length group == count)
--   guard (all (== size) (map sum group))
--   return group
-- 
--  where
--    size = sum presents `div` count

-- groupsTotalling :: Weight -> Weights -> [Weights]
-- groupsTotalling target ws = brute (Set.fromList ws)
--   where
--     brute :: Set.Set Weight -> [Weights]
--     brute bag 
--       | Set.null bag = []
--       | otherwise = do
--         b <- Set.toList bag
--         let rest = brute (Set.delete b bag)
-- 
--         option <- map (b :) rest ++ rest
--         guard (sum option == target)
--         return option

groupsTotalling :: Weight -> Weights -> [Weights]
groupsTotalling 0 _ = [empty]
groupsTotalling _ [] = empty
groupsTotalling target (w : ws)
  | w <= target = with ++ without
  | otherwise = without

 where
   with = map (w :) (groupsTotalling (target - w) ws)
   without = groupsTotalling target ws

optimize :: Weights -> Score
optimize ws = (length ws, product ws)

main :: IO ()
main = do
  input <- map read . lines <$> readFile "24.input"
  -- print . minimumBy (compare `on` optimize) $ equalGroups input 3

  -- let input = [1,2,3,4,5,7,8,9,10,11]
  print . product . minimumBy (compare `on` optimize) $ groupsTotalling (sum input `div` 4) input
  
  -- print (groupsTotalling 6 [1, 2, 4, 5, 6])
