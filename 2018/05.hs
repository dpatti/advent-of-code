import Advent

isReactive :: Char -> Char -> Bool
isReactive a b = toLower a == toLower b && a /= b

react :: String -> String
react = churn f
  where
    f cont x y
      | isReactive x y = cont []
      | otherwise = x : cont [y]

allVariants :: String -> [String]
allVariants xs = do
  target <- ['a' .. 'z']
  guard $ target `elem` xs
  return $ filter ((/= target) . toLower) xs

main :: IO ()
main = solve parse part1 part2
  where
    parse = dropWhileEnd isSpace
    part1 = length . last . iterateFix react
    part2 = minimum . map part1 . allVariants
