module Advent.State where

import Control.Monad.State.Strict

iterateState :: State s a -> s -> [a]
iterateState eff s = a : iterateState eff s'
  where
    (a, s') = runState eff s

updateWith :: a -> State a b -> a
updateWith s = (`execState` s)
