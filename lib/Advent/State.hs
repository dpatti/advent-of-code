module Advent.State where

import Control.Monad.State.Strict

repeatState :: s -> State s a -> [a]
repeatState s eff = a : repeatState s' eff
  where (a, s') = runState eff s

findState :: s -> State s (Maybe a) -> a
findState s eff = evalState loop s
  where
    loop = eff >>= maybe loop return

iterateState :: s -> a -> (a -> State s a) -> [a]
iterateState s a f = a : iterateState s' a' f
  where (a', s') = runState (f a) s

updateWith :: s -> State s a -> s
updateWith s = (`execState` s)
