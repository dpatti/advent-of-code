import Advent
import qualified Data.Map as Map
import Data.Map.Strict (Map)

type Art = [[State]]
data State = On | Off deriving (Eq, Ord)
instance Show State where
  show On = "#"
  show Off = "."

data Rule = Rule [[State]] [[State]] deriving Show
type Rules = Map [[State]] [[State]]

showArt :: Art -> String
showArt = intercalate "/" . map (concatMap show)

-- 0 1 2    6 3 0
-- 3 4 5 -> 7 4 1
-- 6 7 8    8 5 2
rotateArt :: Art -> Art
rotateArt = transpose . reverse

buildRules :: [Rule] -> Rules
buildRules rs = Map.fromList $ do
  (Rule match output) <- rs
  match <- take 4 (iterate rotateArt match)
  match <- [match, flipH match]
  match <- [match, flipV match]
  return (match, output)

 where
   flipH = map reverse
   flipV = reverse

section :: Parser Art
section =
  many ((char '.' $> Off) <|> (char '#' $> On))
  `sepBy` char '/'

ruleParser :: Parser [Rule]
ruleParser = many $ do
  match <- section
  _ <- string " => "
  output <- section
  _ <- newline
  return $ Rule match output

initial :: Art
initial = parseWith section ".#./..#/###"

transform :: Rules -> Art -> Art
transform rules art =
  Map.lookup art rules
  & fromMaybe (error ("No transform for: \n" ++ showArt art))

sliceBy :: Int -> (Art -> Art) -> Art -> Art
sliceBy n f art = do
  row <- chunks n art
  transpose . concatMap f . chunks n . transpose $ row

size :: Art -> Int
size = length

step :: Rules -> Art -> Art
step rules art =
  sliceBy chunkSize (transform rules) art

  where
    chunkSize = if isEven (size art) then 2 else 3
    isEven x = x `mod` 2 == 0

ons :: Art -> Int
ons = count On . concat

main :: IO ()
main = solve parse part1 part2
  where
    parse = parseWith ruleParser
    -- part1 _ = intercalate "\n\n" $ map showArt $ take 4 (iterate rotateArt initial)
    part1 = ons . (!! 5) . flip iterate initial . step . buildRules
    part2 = ons . (!! 18) . flip iterate initial . step . buildRules
