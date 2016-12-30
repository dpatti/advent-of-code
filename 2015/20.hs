import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Sequence
import Debug.Trace

traceShowIf :: Show a => Bool -> a -> b -> b
traceShowIf True a b = traceShow a b
traceShowIf False _ b = b

factorize :: Int -> [Int]
factorize n = 1 : n : filter divides [2..n `div` 2]
  where divides m = n `mod` m == 0

factorize' :: Int -> [Int]
factorize' = factorsFrom 1
  where
    factorsFrom :: Int -> Int -> [Int]
    factorsFrom m n
      | m * m > n = []
      | m * m == n = m : factorsFrom (m + 1) n
      | n `mod` m == 0 = m : n `div` m : factorsFrom (m + 1) n
      | otherwise = factorsFrom (m + 1) n

maxGifts = 50

multiples :: Int -> [Int]
multiples n = multiples' maxGifts 0
  where
    multiples' 0 0 = []
    multiples' m 0 = n : multiples' (m - 1) (n - 1)
    multiples' m s = 0 : multiples' m (s - 1)

-- multiples :: Int -> [Int]
-- multiples n = sepBy spacer $ replicate maxGifts n
--   where
--     spacer :: [Int]
--     spacer = replicate (n - 1) 0
--
--     -- sepBy [0,0] [1,2,3] == [1,0,0,2,0,0,3,0,0]
--     sepBy :: [a] -> [a] -> [a]
--     sepBy items = concatMap (:items)

-- mus :: [[Int]]
-- mus = mus' 1
--   where
--     mus' n = multiples n : mus' (n + 1)

combined :: [Int]
combined = generate 1 inf
  where
    generate :: Int -> [Int] -> [Int]
    generate n (s : state) =
      traceShowIf (n `mod` 1000 == 0) n $
      s : generate (n + 1) (next n state)
    next :: Int -> [Int] -> [Int]
    -- turns [1, 1, 1, 0, 0, ...]
    -- with [2, 0, 2, 0, 2]
    -- into [3, 1, 3, 0, 2]
    next n xs = zipWith (+) (multiples n) (xs ++ inf)
    inf = repeat 0

type Divisor = (Int, Int)

combined2 :: [Int]
combined2 = generateAll 1 []
  where
    generateAll :: Int -> [Divisor] -> [Int]
    generateAll n divs =
      let (sum, newDivs) = foldl' (takeDivs n) (0, []) ((n, maxGifts) : divs)
      in
        traceShowIf (n `mod` 1000 == 0) n $
        sum : generateAll (n + 1) newDivs

    takeDivs :: Int -> (Int, [Divisor]) -> Divisor -> (Int, [Divisor])
    takeDivs n result@(sum, divs) div@(m, count)
      | count == 0 = result
      | n `mod` m == 0 = (sum + m, (m, count - 1) : divs)
      | otherwise = (sum, div : divs)

combined3 :: [Int]
combined3 = generateAll 1 Sequence.empty

generateAll :: Int -> Sequence.Seq Divisor -> [Int]
generateAll n init =
  -- Add our new divisor onto the end, taking one off
  let divs = init Sequence.|> (n, maxGifts - 1) in
  -- Compute the new divisor list
  let (sum, newDivs) = takeDivs divs in

  traceShowIf (n `mod` 1000 == 0) n $
  -- Add the one we took back on
  (sum + n) : generateAll (n + 1) newDivs

  where
    -- Takes a sequence and pulls out all multiples of n and sums them up
    takeDivs :: Sequence.Seq Divisor -> (Int, Sequence.Seq Divisor)
    takeDivs divs = case Sequence.viewl divs of
      -- We should never totally exhaust the sequence
      Sequence.EmptyL -> error "Nothing in sequence"
      -- This should be the only real case
      (div@(m, count) Sequence.:< rest)
        -- No more left, omit
        | count == 0 -> takeDivs rest
        -- This is a workaround for 1 and 2
        | m == n -> (0, divs)
        -- We're past the threshold, so we can stop
        | m * 2 > n -> (0, divs)
        -- Otherwise, we have to choose whether or not to keep this divisor
        | otherwise ->
          let (sum, newDivs) = takeDivs rest in

          if n `mod` m == 0
          then (sum + m, (m, count - 1) Sequence.<| newDivs)
          else (sum, div Sequence.<| newDivs)

      -- (div@(m, count) Sequence.:< rest) ->
      --   if  n `mod` m == 0
      --   if  n `mod` m == 0
      --   then takeDivs n (sum + m) ((m, count - 1)

    --takeDivs n result@(sum, divs) div@(m, count)
    --  | count == 0 = result
    --  | n `mod` m == 0 = (sum + m, (m, count - 1) : divs)
    --  | otherwise = (sum, div : divs)

-- combineOffsetCol :: (Int -> [Int]) -> [Int]
-- combineOffsetCol gen = col 1 [[]] 
--   where
--     col n prev = sum . map head $ prev

-- [1]
-- [1 2]
-- [1 0 3]
-- [1 2 0 4]

-- []
-- 1 -> 1, [2: 1]
-- 2 -> 3, [3: 1, 4: 2]
-- 3 -> 4, [4: 2, 4: 1, 6: 3]

type SumDivs = Map.Map Int Int

combined4 :: [Int]
combined4 = generate 1 Map.empty
  where
    generate :: Int -> SumDivs -> [Int]
    generate n mults =
      traceShowIf (n `mod` 1000 == 0) n $
      let mults' = updateAll n mults in
      let gifts = n + fromMaybe 0 (Map.lookup n mults) in
      gifts : generate (n + 1) (Map.delete n mults')

    updateAll :: Int -> SumDivs -> SumDivs
    updateAll n mults =
      let update maybe = return $ n + fromMaybe 0 maybe in
      foldr (Map.alter update . (* n)) mults [1..maxGifts]

main :: IO ()
main = do
  input <- read <$> getContents

  let op = (+ 1) . fromJust . findIndex (> input)
  -- let op = take 10
  -- print . op . map sum . tail . transpose $ mus input-- . take 10 . multiples $ input
  
  -- print . fromJust . findIndex (> input) . map (*11) $ combined
  -- print . take input $ combined2

  print . take 55 . map (*11) $ combined4
  print . (1+) . fromJust . findIndex (> input) . map (*11) $ combined4

  -- print . take input $ combined
  -- let nats = 1 : map (+1) nats
  -- print . map ((*10) . sum . factorize) $ nats
  -- print . op . map (sum . factorize') $ [1..]
  -- print . take 10 . tail . transpose $ mus
  -- print (multiples input)
  -- print . op . map ((*11) . sum) . transpose $ mus input
