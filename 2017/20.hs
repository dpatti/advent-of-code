{-# LANGUAGE NamedFieldPuns #-}
import Advent
import qualified Data.Map as Map

newtype Vector3 = Vector3 (Int, Int, Int) deriving (Eq, Ord, Show)

add :: Vector3 -> Vector3 -> Vector3
add (Vector3 (x1, y1, z1)) (Vector3 (x2, y2, z2)) =
  Vector3 (x1 + x2, y1 + y2, z1 + z2)

data Particle = Particle
  { particleId :: Int
  , position :: Vector3
  , velocity :: Vector3
  , acceleration :: Vector3 } deriving Show

-- Woah, weird compiler bug with irrefutable matches and ApplicativeDo in <=8.2,
-- so using Monad for now (https://ghc.haskell.org/trac/ghc/ticket/14105)
vectorParser :: Parser Vector3
vectorParser = do
  char '<'
  [x, y, z] <- signed space decimal `sepBy` char ','
  char '>'
  return $ Vector3 (x, y, z)

-- p=<676,-2628,-1641>, v=<-8,52,59>, a=<-4,13,5>
particleParser :: Int -> Parser Particle
particleParser particleId = do
  string "p="
  position <- vectorParser
  string ", v="
  velocity <- vectorParser
  string ", a="
  acceleration <- vectorParser
  return Particle { particleId, position, velocity, acceleration }

score :: Particle -> Int
score Particle { position = Vector3 (x, y, z) } = abs x + abs y + abs z

step :: Particle -> Particle
step p@Particle { position, velocity, acceleration } =
  let velocity' = add velocity acceleration
   in p
    { position = add position velocity'
    , velocity = velocity'
    , acceleration }

rank :: [Particle] -> [Int]
rank = map particleId . take 5 . sortBy (compare `on` score)



-- Remove any particles that collided
reduce :: [Particle] -> [Particle]
reduce = catMaybes . Map.elems . Map.fromListWith (\_ _ -> Nothing) . map byPosition
  where byPosition p = (position p, Just p)

main :: IO ()
main = solve parse part1 part2
  where
    -- it feels kind of cheating to pick an arbitrary number of steps (1000)
    -- instead of finding some sort of stable point?
    parse = zipWith (parseWith . particleParser) [0..] . lines
    part1 = head . map rank . drop 1000 . iterate (map step)
    part2 = length . head . drop 1000 . iterate (reduce . map step)
