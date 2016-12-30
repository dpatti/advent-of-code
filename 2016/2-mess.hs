data Input = U | D | L | R deriving (Show, Read)


-- 1 2 3
-- 4 5 6
-- 7 8 9
--
--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D

bounded m n
 | n < 1 = m
 | n > 9 = m
 | otherwise = n


eval' 3 U = 1
eval' 6 U = 2
eval' 7 U = 3
eval' 8 U = 4
eval' 10 U = 6
eval' 11 U = 7
eval' 12 U = 8
eval' 13 U = 11

eval' 1 D = 3
eval' 2 D = 6
eval' 3 D = 7
eval' 4 D = 8
eval' 6 D = 10
eval' 7 D = 11
eval' 8 D = 12
eval' 11 D = 13

eval' 3 L = 2
eval' 4 L = 3
eval' 6 L = 5
eval' 7 L = 6
eval' 8 L = 7
eval' 9 L = 8
eval' 11 L = 10
eval' 12 L = 11

eval' 2 R = 3
eval' 3 R = 4
eval' 5 R = 6
eval' 6 R = 7
eval' 7 R = 8
eval' 8 R = 9
eval' 10 R = 11
eval' 11 R = 12



eval' n U = n
eval' n D = n
eval' n L = n
eval' n R = n


-- eval' n U = bounded n (n - 3)
-- eval' n D = bounded n (n + 3)
-- eval' n L = bounded n $ fancy n (pred)
-- eval' n R = bounded n $ fancy n (succ)--(((n `div` 3) + 1 + 1) * 3)

-- eval' 1 L = 1
-- eval' 2 L = 1
-- eval' 3 L = 2
-- eval' 4 L = 4
-- eval' 5 L = 4
-- eval' 6 L = 5
-- eval' 7 L = 7
-- eval' 8 L = 7
-- eval' 9 L = 8
-- 
-- eval' 1 R = 2
-- eval' 2 R = 3
-- eval' 3 R = 3
-- eval' 4 R = 5
-- eval' 5 R = 6
-- eval' 6 R = 6
-- eval' 7 R = 8
-- eval' 8 R = 9
-- eval' 9 R = 9
-- 
-- 
-- eval' n L = n
-- eval' n R = n

fancy n f = (f (div3 - 1)) * 3 + rem
  where
    div3 = n `div` 3
    rem = n `mod` 3

-- 5 div 3 = 1
-- + 1 = 2
-- - 1 = 1
-- * 3 = 3

eval :: Int -> [Input] -> Int
eval = foldl eval'

main = do
  input <- lines <$> getContents
  let seqs = map (map (read . pure)) input
  print $ scanl eval 5 seqs
