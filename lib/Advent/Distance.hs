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

instance Semigroup Distance where
  Unreachable <> _ = Unreachable
  _ <> Unreachable = Unreachable
  Distance a <> Distance b = Distance (a + b)

instance Monoid Distance where
  mempty = Distance 0
