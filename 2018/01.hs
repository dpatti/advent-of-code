import Advent
import qualified Data.Set as Set

firstDup :: [Int] -> Int
firstDup = loop mempty
  where
    loop seen (x:xs) =
      if x `Set.member` seen
         then x
         else loop (Set.insert x seen) xs

main :: IO ()
main = solve parse part1 part2
  where
    parse = map (parseWith signedInt) . lines
    part1 = foldl' (+) 0
    part2 = firstDup . scanl (+) 0 . cycle
