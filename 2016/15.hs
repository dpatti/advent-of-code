import Data.List
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String
import Debug.Trace

data Disc = Disc Int Int deriving Show

parseDisc :: String -> Disc
parseDisc = fromRight . parse discParser "stdin"
  where
    discParser :: Parser Disc
    discParser = do
      string "Disc #"
      decimal
      string " has "
      max <- fromIntegral <$> decimal
      string " positions; at time=0, it is at position "
      init <- fromIntegral <$> decimal
      string "."

      return $ Disc max init

fromRight (Left e) = error $ "fromLeft: " ++ show e
fromRight (Right v) = v

rotate :: [Disc] -> [(Int, [Int])]
rotate ds = map time' [0..]
  where
    time' :: Int -> (Int, [Int])
    time' n = (n, [(init + n) `mod` max | (Disc max init) <- ds])

offset :: [Disc] -> [Disc]
offset ds = do
  (Disc max init, t) <- zip ds [1..]

  return $ Disc max ((init + t) `mod` max)

main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

  where
    parse :: String -> [Disc]
    parse = map parseDisc . lines
    solve1 = solve
    solve2 = solve . (++ [Disc 11 0])
    solve = find (all (== 0) . snd) . rotate . offset

