{-# LANGUAGE PackageImports, ViewPatterns #-}

import qualified "cryptonite" Crypto.Hash as Hash
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord
-- import qualified Crypto.Hash.MD5 as MD5
import Numeric
import qualified Data.ByteString.Char8 as BS
import Debug.Trace

-- lpad :: Int -> Char -> String -> String
-- lpad count char str = replicate padding char ++ str
--   where padding = count - length str

-- hash :: String -> String
-- hash = lpad 32 '0' . BS.foldr toHexString mempty . MD5.hash . BS.pack . traceShowId
--   where
--     toHexString :: Char -> String -> String
--     toHexString = showHex . ord

hash :: String -> String
hash = show . Hash.hashWith Hash.MD5 . BS.pack

incrementalHashes :: String -> [String]
incrementalHashes id = map (hash . combine id) [1..] -- [3 000 000..]
  where
    combine :: String -> Int -> String 
    combine id i = id ++ show i

solve :: String -> String
solve = take 8 . map (!! 5) . filter interesting . incrementalHashes
  where
    interesting hash = all (== '0') (take 5 hash)
    toHexChar = head . ($ mempty) . showHex

solve' :: String -> String
solve' = track . map pair . filter valid . filter interesting . incrementalHashes
  where
    track = order . take 8 . nubBy (((== EQ) .) . comparing fst)
    -- track = order . Map.toList . head . dropWhile ((< 8) . Map.size) . scanl fill Map.empty
    fill map (key, value) = Map.insertWith (flip const) key value map

    order :: [(Int, Char)] -> String
    order = map snd . sortBy (comparing fst)

    pair :: String -> (Int, Char)
    pair = (,) <$> (read . pure . (!! 5)) <*> (!! 6)
    interesting hash = all (== '0') (take 5 hash)
    valid ((!! 5) -> n) = n < '8'
    toHexChar = head . ($ mempty) . showHex

main = do
  input <- takeWhile (not . isSpace) <$> getContents
  -- putStrLn . solve $ input
  putStrLn . solve' $ input
