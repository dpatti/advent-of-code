module Advent.String where

lpad :: Int -> Char -> String -> String
lpad n char str =
  replicate (length str - n) char ++ str

rpad :: Int -> Char -> String -> String
rpad n char str =
  str ++ replicate (length str - n) char
