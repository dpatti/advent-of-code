module Advent.Distance where

data Distance = Unreachable | Distance Int deriving Eq

fromDistance Unreachable = error "Unreachable!"
fromDistance (Distance n) = n

instance Show Distance where
  show Unreachable = "inf"
  show (Distance n) = show n

instance Ord Distance where
  compare Unreachable Unreachable = EQ
  compare Unreachable _ = GT
  compare _ Unreachable = LT
  compare (Distance a) (Distance b) = compare a b

instance Monoid Distance where
  mempty = Distance 0
  mappend Unreachable _ = Unreachable
  mappend _ Unreachable = Unreachable
  mappend (Distance a) (Distance b) = Distance (a + b)
