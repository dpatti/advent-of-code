module Advent.Debug where

import Debug.Trace

traceShowIdWith :: (Show b) => (a -> b) -> a -> a
traceShowIdWith f x = traceShow (f x) x

traceIdWith :: (a -> String) -> a -> a
traceIdWith f x = trace (f x) x
