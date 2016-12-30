{-# LANGUAGE ViewPatterns #-}

import Advent
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

data Operation = SwapPosition Int Int
               | SwapLetter Char Char
               | Rotate Direction Int
               | RotateByLetter Char
               | ReversePositions Int Int
               | MovePosition Int Int
               deriving Show

parseOperation :: String -> Operation
parseOperation = parseOperation' . words
  where
    parseOperation' ["swap", "position", read -> x, "with", "position", read -> y] =
      SwapPosition x y
    parseOperation' ["swap", "letter", head -> x, "with", "letter", head -> y] =
      SwapLetter x y
    parseOperation' ["rotate", parseDirection -> d, read -> x, _] =
      Rotate d x
    parseOperation' ["rotate", "based", "on", "position", "of", "letter", head -> x] =
      RotateByLetter x
    parseOperation' ["reverse", "positions", read -> x, "through", read -> y] =
      ReversePositions x y
    parseOperation' ["move", "position", read -> x, "to", "position", read -> y] =
      MovePosition x y
    parseDirection "left" = DLeft
    parseDirection "right" = DRight

type Swap a = (a, Int) -> a

swapBy :: Swap a -> [a] -> [a]
swapBy s = map s . (`zip` [0..])

swapPos :: Int -> a -> Swap a
swapPos pos swap (el, idx)
  | pos == idx = swap
  | otherwise = el

swapEl :: Eq a => a -> a -> Swap a
swapEl search swap (el, idx)
  | el == search = swap
  | otherwise = el

perform :: String -> Operation -> String
perform password (SwapPosition x y) =
  let first = password !! x
      second = password !! y
   in swapBy (swapPos x second) . swapBy (swapPos y first) $ password
perform password (SwapLetter x y) =
  let first = fromJust (elemIndex x password)
      second = fromJust (elemIndex y password)
   in swapBy (swapPos first y) . swapBy (swapPos second x) $ password
perform password (Rotate d x) = rotate d x password
perform password (RotateByLetter x) =
  let i = fromJust (elemIndex x password)
   in rotate DRight (min 1 $ i `div` 4) . rotate DRight i . rotate DRight 1 $ password
perform password (ReversePositions x y) =
  let [before, target, after] = splitPlacesBlanks [x, y - x + 1, length password] password
   in before ++ reverse target ++ after
perform password (MovePosition x y) =
  let letter = password !! x
      removed = take x password ++ drop (x + 1) password
   in take y removed ++ [letter] ++ drop y removed

unperform :: String -> Operation -> String
unperform password (SwapPosition x y) = perform password (SwapPosition y x)
unperform password (SwapLetter x y) = perform password (SwapLetter y x)
unperform password (Rotate d x) = perform password (Rotate (inverse d) x)
unperform password (RotateByLetter x) =
  let i = fromJust (elemIndex x password)
   in rotate DLeft (reverseRBL i) password
unperform password (ReversePositions x y) = perform password (ReversePositions x y)
unperform password (MovePosition x y) = perform password (MovePosition y x)

reverseRBL 0 = 9
reverseRBL 1 = 1
reverseRBL 2 = 6
reverseRBL 3 = 2
reverseRBL 4 = 7
reverseRBL 5 = 3
reverseRBL 6 = 8
reverseRBL 7 = 4

-- password = "abcde"
password = "abcdefgh"

main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

 where
   parse = map parseOperation . lines
   solve1 = foldl' perform password
   solve2 = foldr (flip unperform) "fbgdceah"
