import Advent
import Text.Read

factorA, factorB, divisor :: Int
factorA = 16807
factorB = 48271
divisor = 2147483647

generate :: Int -> Int -> [Int]
generate factor previous =
  let next = (previous * factor) `mod` divisor
   in next : generate factor next

parseInits :: (Read a, Num a) => String -> (a, a)
parseInits = pair . mapMaybe readMaybe . words
  where
    pair [a, b] = (a, b)
    pair _ = error "Expected 2-length list"

judge :: Int -> Int -> Bool
judge a b = (a .&. mask) == (b .&. mask)
  where
    mask = shiftL 1 16 - 1

sample1, sample2 :: Int
sample1 = 40000000
sample2 =  5000000
-- sample = 10

multiplesOf :: Int -> [Int] -> [Int]
multiplesOf n = filter ((== 0) . (`mod` n))

main :: IO ()
main = solve parse part1 part2
  where
    parse = parseInits
    part1 (a, b) = countBy (uncurry judge) . take sample1 $
      zip (generate factorA a) (generate factorB b)
    part2 (a, b) = countBy (uncurry judge) . take sample2 $
      zip (multiplesOf 4 $ generate factorA a) (multiplesOf 8 $ generate factorB b)
