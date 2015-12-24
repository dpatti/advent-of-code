direction :: Char -> Int -> Int
direction '(' = succ
direction ')' = pred
direction _ = id

lastFloor :: String -> Int
lastFloor = foldr direction 0

basementCommand :: String -> Int
basementCommand = length . takeWhile (>= 0) . scanl (flip direction) 0

main :: IO ()
main = do
  input <- getContents
  print $ lastFloor input
  print $ basementCommand input
