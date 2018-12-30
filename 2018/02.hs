import Advent
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow

toFreqs :: String -> Set Int
toFreqs = Set.fromList . map length . group . sort

withElem :: Ord a => a -> [Set a] -> Int
withElem n = countBy (Set.member n)

diffs :: String -> String -> Int
diffs s = count False . zipWith (==) s

sames :: String -> String -> String
sames s = map fst . filter (uncurry (==)) . zip s

firstTwo :: [a] -> Maybe (a, a)
firstTwo [a, b] = Just (a, b)
firstTwo _ = Nothing

main :: IO ()
main = solve parse part1 part2
  where
    parse = lines
    part1 = uncurry (*) . (withElem 2 &&& withElem 3) . map toFreqs
    part2 = fmap (uncurry sames) . findElemBy (uncurry diffs) 1 . mapMaybe firstTwo . combinations 2
