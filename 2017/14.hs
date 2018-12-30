import Advent
import Advent.Ring

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

hash :: String -> [Int]
hash = reduce . compute . parse
    where
      -- from 10.hs
      parse = concat . replicate 64 . (++ padding) . map ord
      padding = [17, 31, 73, 47, 23]
      compute = toList . rewind . foldl hashRound (initial, 0, 0)
      reduce = map (foldl1 xor) . chunks 16
      initial = fromList [0..255]
      rewind (ring, moved, _) = skip ((-moved) `mod` length ring) ring
      hashRound (ring, moved, s) n =
        (skip s . splice n reverse $ ring, moved + s + n, s + 1)

countOnes :: [Int] -> Int
countOnes = sum . map popCount

suffix :: String -> Int -> String
suffix s n = s ++ "-" ++ show n

toByteString :: [Int] -> ByteString
toByteString = ByteString.pack . map fromIntegral

countRegions :: ByteString -> Int
countRegions memory = tally . sort . filter (> 0) . map countNeighbors $ addresses
  where
    blockSize = 128

    addresses :: [Int]
    addresses = [0 .. blockSize * blockSize - 1]
    -- addresses = [0, 1, 2, 3, 4, 5, 6, 7, 8] ++ [128 .. 136]

    addrToCoord :: Int -> Coord
    addrToCoord addr = Coord (addr `div` blockSize) (addr `mod` blockSize)

    coordToAddr :: Coord -> Int
    coordToAddr (Coord x y) = x * blockSize + y

    countNeighbors :: Int -> Int
    countNeighbors address =
      length . nub . bfsMemo (used . addrNeighbors) . used $ [address]

    used = filter isUsed

    isUsed :: Int -> Bool
    isUsed addr = testBit (ByteString.index memory byte) bit
      where
        byte = addr `div` 8
        bit = 7 - (addr `mod` 8)

    addrNeighbors =
      map coordToAddr . filter (coordInBounds lower upper) . neighbors . addrToCoord

    lower = addrToCoord 0
    upper = addrToCoord (blockSize * blockSize - 1)

    tally :: [Int] -> Int
    tally [] = 0
    tally (x:xs) = 1 + tally (drop (x - 1) xs)

main :: IO ()
main = solve parse part1 part2
  where
    parse = traverse (flip suffix) [0..127] . filter (/= '\n')
    part1 = countOnes . concatMap hash
    part2 = countRegions . ByteString.concat . map (toByteString . hash)
