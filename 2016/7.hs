import Data.Attoparsec.Text hiding (take)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Debug.Trace

data Address = Address { outer :: [String], inner :: [String] } deriving Show

fromRight (Left e) = error e
fromRight (Right v) = v

parseAddress :: T.Text -> Address 
parseAddress = (fromRight .) <$> parseOnly $ do
  pairs <- many1 $ do
    outer <- many1 (notChar '[')
    char '['
    inner <- many1 (notChar ']')
    char ']'
    return (outer, inner)
  final <- many1 anyChar

  return Address { outer = final : map fst pairs, inner = map snd pairs }

tcp :: Address -> Bool
tcp (Address outer inner) = any abba outer && not (any abba inner)
  where
    abba :: String -> Bool
    abba xs@(a:b:c:d:_)
      | a == d && b == c && a /= b = True
      | otherwise = abba (tail xs)
    abba _ = False

ssl :: Address -> Bool
ssl (Address outers inners) = any hasBAB abas
  where
    abas = concatMap aba outers

    aba :: String -> [String]
    aba xs@(a:b:c:_)
      | a == c && a /= b = [a, b, c] : aba (tail xs)
      | otherwise = aba (tail xs)
    aba _ = []

    hasBAB aba = any (bab `isInfixOf`) inners
      where
        bab = let [a, b, _] = aba in [b, a, b]

main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

  where
    parse = map (parseAddress . T.pack) . lines
    solve1 = length . filter tcp
    solve2 = length . filter ssl
