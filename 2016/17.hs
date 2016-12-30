{-# LANGUAGE OverloadedLists, NamedFieldPuns, PackageImports, ViewPatterns #-}

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.ByteString.Char8 as BS
import qualified "cryptonite" Crypto.Hash as Hash
import Debug.Trace

newtype Coord = Coord (Int, Int) deriving (Eq, Show)

modifyEach :: (Int -> Int) -> Coord -> Coord
modifyEach f (Coord (x, y)) = Coord (f x, f y)

data World = World { pos :: Coord
                   , path :: String
                   , passcode :: String
                   } deriving Show
data Direction = U | D | L | R deriving Show

bound :: (Int, Int) -> Int -> Int
bound (lower, upper) = min upper . max lower

inBounds :: (Int, Int) -> Coord -> Maybe Coord
inBounds (min, max) c@(Coord (x, y))
  | x < min || x > max || y < min || y > max = Nothing
  | otherwise = Just c

move :: Direction -> Coord -> Maybe Coord
move dir (Coord (x, y)) = inBounds (0, 3) $
  case dir of
    U -> Coord (x, y - 1)
    D -> Coord (x, y + 1)
    L -> Coord (x - 1, y)
    R -> Coord (x + 1, y)

bfs :: Show a => (a -> [a]) -> [a] -> [a]
bfs _ [] = []
bfs getNeighbors nodes = nodes ++ traverse (traceShow (length next) next)
  where
    traverse = bfs getNeighbors
    next = concatMap getNeighbors nodes

hash :: String -> String
hash = show . Hash.hashWith Hash.MD5 . BS.pack

roomInfo :: String -> [Direction]
roomInfo = catMaybes . zipWith switch [U, D, L, R] . take 4 . hash
  where
    switch dir code
      | code >= 'b' = Just dir
      | otherwise = Nothing

search :: World -> [World]
search w@World { pos, path, passcode } = do
  guard (pos /= Coord (3, 3))
  let allowed = roomInfo (passcode ++ path)
  dir <- allowed
  pos' <- maybeToList (move dir pos)
  return $ w { pos = pos', path = path ++ show dir }

victory :: World -> Bool
victory = (vault ==) . pos
  where vault = Coord (3, 3)

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast f = foldl' (\res x -> if f x then Just x else res) Nothing

hwm :: (Ord a, Show a) => [a] -> [a]
hwm (x:xs) = x : hwm (filter (> x) xs)
  where
    track :: Ord a => a -> [a] -> [a]
    track x xs = x : filter (< x) xs

    hwm' :: (Ord a, Show a) => a -> [a] -> [a]
    hwm' _ [] = []
    hwm' (traceShowId -> max) (x:xs)
      | x > max = x : hwm' x xs
      | otherwise = hwm' max xs

main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

 where
   parse = head . lines
   solve1 = fmap path . find victory . bfs search . (:[]) . mkWorld
   solve2 = fmap (length . path) . findLast victory . bfs search . (:[]) . mkWorld
   mkWorld = World (Coord (0, 0)) ""
