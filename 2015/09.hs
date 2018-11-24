import           Control.Applicative
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe

type Place = String
data Path = Path Place Place Int
type Paths = Map.Map (Place, Place) Int

indexPaths :: [Path] -> Paths
indexPaths = foldl' addPath Map.empty 
  where
    addPath paths (Path a b dist) = Map.insert (a, b) dist paths

parseRoute :: String -> Path
parseRoute str = case words str of
  [a, "to", b, "=", dist] -> Path a b (read dist)

cost :: Paths -> [Place] -> Int
cost paths [final] = 0
cost paths (a : b : rest) = leg + cost paths (b : rest)
  where leg = fromJust (Map.lookup (a, b) paths <|> Map.lookup (b, a) paths)

main :: IO ()
main = do
  paths <- indexPaths . map parseRoute . lines <$> getContents
  let places = nub . concatMap pairs . Map.keys $ paths
  print . maximum . map (cost paths) . permutations $ places
  where pairs (a, b) = [a, b]
