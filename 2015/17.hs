import Data.List

eggnog = 150

ranges :: Int -> Int -> Int -> [[Int]]
ranges 0 min max = [[]]
ranges n min max = do
  v <- [min..max]
  map (v:) $ ranges (n - 1) min max

main :: IO ()
main = do
  containers <- map read . lines <$> getContents
  let useMaps = ranges (length containers) 0 1
  let solns = filter ((== eggnog) . sum) . map (zipWith (*) containers) $ useMaps
  print . length $ solns
  let used = map (length . filter (> 0)) solns
  print used
  print . length . filter (== minimum used) $ used
