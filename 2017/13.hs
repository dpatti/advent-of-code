import Advent

type Layer = (Int, Int)

layerParser :: Parser Layer
layerParser = do
  layer <- decimal
  string ": "
  range <- decimal
  return (layer, range)

rangeCycle :: Int -> Int
rangeCycle 1 = 1
rangeCycle n = n + (n - 2)

hitchCatches :: Int -> [Layer] -> [Layer]
hitchCatches delay = filter isCaught
  where
    isCaught (psec, range) = (psec + delay) `mod` rangeCycle range == 0

severity :: Layer -> Int
severity = uncurry (*)

main :: IO ()
main = solve parse part1 part2
  where
    parse = map (parseWith layerParser) . lines
    part1 = sum . map severity . hitchCatches 0
    part2 layers = find (null . (`hitchCatches` layers)) [0 ..]
