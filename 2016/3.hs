import Data.List

valid :: [Int] -> Bool
valid [a, b, c] = a + b > c && a + c > b && b + c > a

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice n xs = take n xs : slice n (drop n xs)

main = do
  input <- map (map read . words) . lines <$> getContents
  print . solution $ input
  print . solution . slice 3 . flatten $ input
  print . reshape $ input

  where
    flatten :: [[Int]] -> [Int]
    flatten xss = concatMap (\i -> map (!! i) xss) [0..2]

    reshape = slice 3 . concat . transpose

    solution = length . filter valid
