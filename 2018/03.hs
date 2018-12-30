import Advent
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Rect = Rect Coord Coord
type Id = Int
data Claim = Claim Id Rect

foldRect :: (a -> Coord -> a) -> a -> Rect -> a
foldRect f acc (Rect (Coord x1 y1) (Coord x2 y2)) =
  foldl f acc $ do
    x <- [x1 .. x2]
    y <- [y1 .. y2]
    return $ Coord x y


claimParser :: Parser Claim
claimParser = do
  _ <- char '#'
  cid <- decimal
  _ <- string " @ "
  x <- decimal
  _ <- char ','
  y <- decimal
  _ <- string ": "
  width <- decimal
  _ <- char 'x'
  height <- decimal
  return $ Claim cid (Rect (Coord x y) (Coord (x + width - 1) (y + height - 1)))

imposeWith :: Monoid m => (Id -> m) -> [Claim] -> Map Coord m
imposeWith f = foldl impose mempty
  where
    impose m (Claim cid rect) =
      foldRect (flip $ upsert ((f cid <>) . fromMaybe mempty)) m rect

overlaps :: [Claim] -> Map Coord Int
overlaps = fmap getSum . imposeWith (const (Sum 1))

sets :: [Claim] -> Map Coord (Set Id)
sets = imposeWith Set.singleton

filterDups :: (Ord a) => [Set a] -> Set a
filterDups ids = Set.difference total dups
  where
    total = Set.unions ids
    dups = Set.unions . filter ((> 1) . length) $ ids

main :: IO ()
main = solve parse part1 part2
  where
    parse = map (parseWith claimParser) . lines
    part1 = countBy (> 1) . Map.elems . overlaps
    part2 = filterDups . Map.elems . sets
