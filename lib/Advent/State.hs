module Advent.State where

import Control.Monad.State

updateWith :: a -> State a b -> a
updateWith s = (`execState` s)
