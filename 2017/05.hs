import Advent
import Data.Vector (Vector, (//), (!?))
import qualified Data.Vector as Vector

parse :: String -> Vector Int
parse = Vector.fromList . map read . lines

jumps :: (Int -> Int) -> Vector Int -> [Int]
jumps update inst = loop (0, inst)
  where
    loop (pc, inst) =
      case inst !? pc of
        Nothing -> []
        Just offset ->
          let inst' = inst // [(pc, update offset)] in
          pc : loop (pc + offset, inst')

main :: IO ()
main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input
 where
   solve1 = length . jumps (+1)
   solve2 = length . jumps (\i -> if i >= 3 then i - 1 else i + 1)
