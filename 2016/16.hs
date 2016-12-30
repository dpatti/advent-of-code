type Data = String

accumulate :: Int -> Data -> Data
accumulate n = take n . extend ""
  where
    extend r d =
      let whole = r ++ d
       in d ++ extend whole ("0" ++ (map inverse . reverse $ whole))
    inverse '0' = '1'
    inverse '1' = '0'

checksum :: Int -> Data -> String
checksum bytes d
  | odd bytes = d
  -- | otherwise = checksum (bytes `div` 2) $ map samediff (pairs d)
  | otherwise = checksum (bytes `div` 2) (reduce d)
  where
    reduce [] = []
    reduce (x:y:xs)
      | x == y = '1' : reduce xs
      | x /= y = '0' : reduce xs

    pairs [] = []
    pairs (x:y:xs) = (x, y) : pairs xs
    samediff (x, y)
      | x == y = '1'
      | x /= y = '0'

main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

 where
   parse = head . lines
   solve1 = checksumForSize 272
   solve2 = checksumForSize 35651584
   -- solve2 = length . accumulate 35651584

   checksumForSize = (.) <$> checksum <*> accumulate
