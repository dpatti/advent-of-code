module Advent.State where

import Control.Monad.State.Strict

repeatState :: s -> State s a -> [a]
repeatState s eff = a : repeatState s' eff
  where (a, s') = runState eff s

iterateState :: a -> s -> (a -> State s a) -> [a]
iterateState a s f = a : iterateState a' s' f
  where (a', s') = runState (f a) s

updateWith :: s -> State s a -> s
updateWith s = (`execState` s)
