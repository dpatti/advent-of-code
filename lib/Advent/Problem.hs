module Advent.Problem where

solve :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> IO ()
solve parse part1 part2 = do
  input <- parse <$> getContents
  print (part1 input)
  print (part2 input)

stub :: a -> ()
stub _ = error "<stub>"
