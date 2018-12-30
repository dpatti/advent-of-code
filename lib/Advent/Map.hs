module Advent.Map where

import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

upsert :: Ord k => (Maybe a -> a) -> k -> Map k a -> Map k a
upsert f = Map.alter (Just . f)

insertAppend :: (Ord k, Monoid a) => k -> a -> Map k a -> Map k a
insertAppend k value = upsert ((`mappend` value) . fromMaybe mempty) k

fromListMulti :: (Ord k) => [(k, a)] -> Map k [a]
fromListMulti = Map.fromListWith mappend . map (fmap pure)
