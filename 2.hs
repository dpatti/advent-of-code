type Dimension = [Int]

combinationPairs :: [a] -> [(a, a)]
combinationPairs [] = []
combinationPairs (x:xs) = these ++ those
  where
    these = zip (repeat x) xs
    those = combinationPairs xs

split :: Char -> String -> [String]
split delim = foldr split' [""]
  where
    split' char (str:strs)
      | char == delim =  "" : str : strs
      | otherwise = (char : str) : strs

parseDimension :: String -> Dimension
parseDimension = map read . split 'x'

paperNeeded :: Dimension -> Int
paperNeeded dims = 2 * sum surfaces + slack
  where
    slack = minimum surfaces
    surfaces = map area . combinationPairs $ dims
    area (x, y) = x * y 

ribbonNeeded :: Dimension -> Int
ribbonNeeded dims = wrap + bow
  where
    wrap = minimum perimeters
    bow = product dims
    perimeters = map perimeter . combinationPairs $ dims
    perimeter (x, y) = 2 * (x + y)

main :: IO ()
main = do
  input <- getContents
  let presents = lines input

  print . sum . map (paperNeeded . parseDimension) $ presents
  print . sum . map (ribbonNeeded . parseDimension) $ presents
