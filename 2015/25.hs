start = 20151125

step :: Int -> Int
step x = (x * 252533) `mod` 33554393

tri :: Int -> Int
tri n = n * (n + 1) `div` 2

main = do
  let index = tri col + sum (take (row - 1) [col..]) - 1
  print (codes !! index)
 where
   col :: Int
   row :: Int
   col = 3029
   row = 2947

   -- col = 2
   -- row = 5

   codes = iterate step start
