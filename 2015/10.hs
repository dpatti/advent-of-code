import Data.List

say :: String -> String
say n = concat . concatMap chunk . group $ n
  where
    chunk :: String -> [String]
    chunk str = [show (length str), [head str]]

step :: Int -> String -> String
step 0 seed = seed
step count seed = step (count - 1) (say seed)

main :: IO ()
main = do
  input <- (unwords . words) <$> getContents
  print . length . step 50 $ input
