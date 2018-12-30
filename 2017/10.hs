import Advent
import Advent.Ring

initial :: Ring Int
initial = fromList [0..255]

hashRound :: (Ring Int, Int, Int) -> Int -> (Ring Int, Int, Int)
hashRound (ring, moved, s) n =
  -- Reverse the order of that length of elements in the list, starting with the element at the current position.
  -- Move the current position forward by that length plus the skip size.
  -- Increase the skip size by one.
  (skip s . splice n reverse $ ring, moved + s + n, s + 1)

rewind :: (Ring Int, Int, a) -> Ring Int
rewind (ring, moved, _) = skip ((-moved) `mod` length ring) ring

hash :: [Int] -> Ring Int
hash = rewind . foldl hashRound (initial, 0, 0)

main :: IO ()
main = solve parse part1 part2
  where
    parse = filter (/= '\n')

    parse1 = map read . splitOn ","
    part1 = product . peek 2 . hash . parse1

    padding = [17, 31, 73, 47, 23]
    parse2 = concat . replicate 64 . (++ padding) . map ord
    part2 = toHexString . map (foldl1 xor) . chunks 16 . toList . hash . parse2
