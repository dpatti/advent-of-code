module Advent.Either where

fromRight :: Either String b -> b
fromRight (Left err) = error $ "fromRight: " ++ err
fromRight (Right value) = value
