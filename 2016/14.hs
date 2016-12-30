{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.State
import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe

import qualified Data.ByteString.Char8 as BS
import qualified "cryptonite" Crypto.Hash as Hash
import Data.Function
import Debug.Trace

type Index = Int
type Hash = String

-- It contains three of the same character in a row, like 777. Only consider the first such triplet in a hash.
-- One of the next 1000 hashes in the stream contains that same character five times in a row, like 77777.
repeats :: Int -> Hash -> Maybe Char
repeats n (splitAt n -> (x:xs, ys))
  | length xs + 1 < n = Nothing
  | all (== x) xs = Just x
  | otherwise = repeats n (xs ++ ys)

trips = repeats 3
quints = repeats 5

filterKeys :: [(Index, Hash)] -> [(Index, Hash)]
filterKeys = flip evalState Seq.empty . filterKeys'
  where
    filterKeys' :: [(Index, Hash)] -> State (Seq (Char, (Index, Hash))) [(Index, Hash)]
    filterKeys' (h@(index, hash):hs) =
      case hash of
        -- (quints -> Just quint) -> do
        --   -- traceM $ "quint@" ++ show index ++ " (" ++ hash ++ ")"
        --   -- prune
        --   modify (Seq.dropWhileL $ \(_, (index', _)) -> index - index' > 1000)
        --   matching <- gets (map snd . toList . Seq.filter ((== quint) . fst))
        --   (matching ++) <$> filterKeys' hs
          
        (trips -> Just trip) -> do
          -- traceM $ "trip@" ++ show index ++ " (" ++ hash ++ ")"
          -- modify (Seq.|> (trip, h))
          if any ((== Just trip) . quints . snd) . take 1000 $ hs
             then (h:) <$> filterKeys' hs
             else filterKeys' hs

        _ -> filterKeys' hs

hashFor :: String -> Index -> (Index, Hash)
hashFor salt = (,) <$> id <*> hash . (salt ++) . show

hash = show . Hash.hashWith Hash.MD5 . BS.pack

stretch :: Int -> (Index, Hash) -> (Index, Hash)
stretch 0 (i, h) = (i, h)
stretch n (i, h) = stretch (n - 1) (i, hash h)

main = do
  input <- parse <$> getContents
  -- print . quints $ "asflkjew lfkjewagoia jwoigaw333lkjalksfjalew"
  -- print . solve1 $ input
  print . solve2 $ input

 where
   parse = head . lines
   solve1 = last . take 64 . filterKeys . zipWith (&) [0..] . repeat . hashFor
   solve2 = last . take 64 . filterKeys . zipWith (&) [0..] . repeat . fmap (stretch 2016) . hashFor
