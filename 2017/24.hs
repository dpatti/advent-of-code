{-# LANGUAGE NamedFieldPuns #-}
import Advent hiding (left)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple

type Component = (Int, Int)
type Inventory = Map Int [Int]

data S = S
  { path :: [Component]
  , pins :: Int
  , left :: Inventory
  } deriving Show

parseComponent :: String -> Component
parseComponent = parseWith $
  (,) <$> decimal <* char '/' <*> decimal

removeMulti :: (Ord k, Eq a) => k -> a -> Map k [a] -> Map k [a]
removeMulti key value = Map.adjust (filter (/= value)) key

toInventory :: [Component] -> Inventory
toInventory components =
  Map.fromListWith (++) . map (fmap return) . ((++) <$> id <*> map swap) $ ordered
  where
    ordered = map order components

order :: Component -> Component
order (a, b)
  | b < a = (b, a)
  | otherwise = (a, b)

initialState :: Inventory -> S
initialState inventory = S { path = [], pins = 0, left = inventory }

search :: S -> [S]
search S { path, pins, left } = do
  pins' <- Map.findWithDefault [] pins left
  let left' = left & removeMulti pins pins' & removeMulti pins' pins
  return S { path = (pins, pins') : path, pins = pins', left = left' }

score :: S -> Int
score = sum . map (uncurry (+)) . path

main :: IO ()
main = solve parse part1 part2
  where
    parse = map parseComponent . lines
    part1 = maximum . map score . bridges
    part2 = maximum . map (\b -> (length (path b), score b)) . bridges

    bridges = bfs search . return . initialState . toInventory
