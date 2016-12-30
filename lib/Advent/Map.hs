module Advent.Map where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

upsert :: Ord k => (Maybe a -> a) -> k -> Map k a -> Map k a
upsert f = Map.alter (Just . f)
