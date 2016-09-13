import Data.List

pairs :: String -> [Char]
pairs [] = []
pairs [_] = []
pairs (a:b:rest)
  | a == b = a : pairs rest
  | otherwise = pairs (b:rest)

valid :: String -> Bool
valid password = all ($ password) [hasStraight, notConfusing, hasTwoPairs]
  where
    hasStraight [_, _] = False
    hasStraight (a:b:c:rest)
      | succ a == b && succ b == c = True
      | otherwise = hasStraight (b:c:rest)

    notConfusing = not . any (`elem` "iol")

    hasTwoPairs = (>= 2) . length . nub . pairs

inc :: String -> String
inc = reverse . incLeft . reverse
  where
    incLeft :: String -> String
    incLeft ('z':rest) = 'a' : incLeft rest
    incLeft (c:rest) = succ c : rest

next :: String -> String
next password = if valid incremented
  then incremented
  else next incremented
  where incremented = inc password

main :: IO ()
main = do
  password <- (unwords . words) <$> getContents
  print (next password)
