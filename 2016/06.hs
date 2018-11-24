import Data.List
import Data.Ord

mode :: (Eq a, Ord a) => [a] -> a
mode = head . minimumBy (comparing length) . group . sort

main = do
  input <- parse <$> getContents
  print . solve $ input

  where
    parse = lines
    solve = map mode . transpose
