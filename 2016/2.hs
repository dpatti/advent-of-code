import qualified Data.Bimap as Bimap
import Data.Maybe
import Debug.Trace

data Input = U | D | L | R deriving (Show, Read)
type Coord = (Int, Int)
type Keypad = [(Int, Coord)]

-- 1 2 3
-- 4 5 6
-- 7 8 9
keypad1 = [ (1, (0, 0))
          , (2, (1, 0))
          , (3, (2, 0))
          , (4, (0, 1))
          , (5, (1, 1))
          , (6, (2, 1))
          , (7, (0, 2))
          , (8, (1, 2))
          , (9, (2, 2))
          ]

--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D
keypad2 = [ (1, (2, 0))
          , (2, (1, 1))
          , (3, (2, 1))
          , (4, (3, 1))
          , (5, (0, 2))
          , (6, (1, 2))
          , (7, (2, 2))
          , (8, (3, 2))
          , (9, (4, 2))
          , (10, (1, 3))
          , (11, (2, 3))
          , (12, (3, 3))
          , (13, (2, 4))
          ]

reverseLookup :: Eq a => a -> Bimap.Bimap k a -> Maybe k
reverseLookup n = Bimap.fold f Nothing
  where
    f k v Nothing
      | v == n = Just k
      | otherwise = Nothing
    f _ _ found = found

step :: Input -> Coord -> Coord
step U (x, y) = (x, y - 1)
step D (x, y) = (x, y + 1)
step L (x, y) = (x - 1, y)
step R (x, y) = (x + 1, y)

move :: Keypad -> Int -> Input -> Int
move kp n inp = tryStep inp . n2c $ n
  where
    tryStep inp coord =
      case c2n (step inp coord) of
        Nothing -> fromJust (c2n coord)
        (Just n) -> n

    kp' = Bimap.fromList kp

    n2c :: Int -> Coord
    n2c = (kp' Bimap.!)
    c2n :: Coord -> Maybe Int
    c2n = (`reverseLookup` kp')

main = do
  input <- parse <$> getContents
  print . solution keypad1 $ input
  print . solution keypad2 $ input

  where
    parse = map (map (read . pure)) . lines
    solution kp = drop 1 . scanl (foldl (move kp)) 5
