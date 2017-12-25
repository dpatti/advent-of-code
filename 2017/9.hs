import Advent

data Segment = Group [Segment] | Garbage Int
type Stream = [Segment]

parseStream :: Parser Stream
parseStream = parseSegment `sepBy` char ','

parseSegment :: Parser Segment
parseSegment = parseGroup <|> parseGarbage

parseGroup :: Parser Segment
parseGroup = do
  char '{'
  stream <- parseStream
  char '}'
  return (Group stream)

parseGarbage :: Parser Segment
parseGarbage = do
  char '<'
  n <- sum <$> many piece
  char '>'
  return (Garbage n)
  where
    piece =
      (char '!' *> anyChar $> 0)
      <|> (notChar '>' $> 1)

scoreStream :: Stream -> Int
scoreStream = sum . map (scoreSegment 1)
  where
    scoreSegment :: Int -> Segment -> Int
    scoreSegment _ (Garbage _) = 0
    scoreSegment value (Group segments) =
      value + sum (map (scoreSegment (value + 1)) segments)

countGarbage :: Stream -> Int
countGarbage = sum . map tally
  where
    tally (Garbage n) = n
    tally (Group segments) = countGarbage segments

main :: IO ()
main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

 where
   parse = parseWith parseStream
   solve1 = scoreStream
   solve2 = countGarbage
