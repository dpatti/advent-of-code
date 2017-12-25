module Advent.Numeric where

import Advent.String
import Numeric

toHexString :: [Int] -> String
toHexString = concatMap (lpad 2 '0' . ($ "") . showHex)
