module Advent.Arrow where

import Control.Arrow

both :: Arrow a => a b c -> a (b, b) (c, c)
both f = f *** f
