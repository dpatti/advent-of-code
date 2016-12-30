{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Advent.Grid where

import Advent.Coord
import Advent.Distance
import Advent.List
import Advent.Search
import Advent.State
import Control.Monad
import Control.Monad.State
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

data Grid a = Grid { tiles :: Map Coord a
                   , poi :: Map a Coord
                   , distance :: Coord -> Coord -> Distance
                   }

buildGrid :: (Coord -> a) -> (Coord -> [Coord]) -> Int -> Int -> Grid a
buildGrid f neighbors xMax yMax = Grid { tiles, poi, distance }
  where
    tiles = foldr (\loc -> Map.insert loc (f loc)) Map.empty $ do
      x <- [0..xMax]
      y <- [0..yMax]

      return $ Coord x y

    poi = undefined

    distance from to = distances ! (from, to)

    locs = Map.elems poi

    distances :: Map (Coord, Coord) Distance
    distances = updateWith Map.empty $
      forM (combinations 2 locs) $ \[from, to] -> do
        let distance = path from to

        modify (Map.insert (from, to) distance)
        modify (Map.insert (to, from) distance)

    path :: Coord -> Coord -> Distance
    path from to =
      maybe Unreachable Distance
      . fmap snd
      . findElemBy fst to
      . bfsMemoBy fst step
      $ [(from, 0)]

    step :: (Coord, Int) -> [(Coord, Int)]
    step (coord, s) =
      map (, s+1) . neighbors $ coord
