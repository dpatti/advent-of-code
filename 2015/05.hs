import Data.List

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

countNice :: [String] -> Int
countNice = length . filter isNice
  where
    isNice str = hasVowels str && doubleLetters str && noBad str

    hasVowels = (>=3) . length . filter isVowel
    doubleLetters = (>=1) . length . filter (>=2) . map length . group 

    noBad [] = True
    noBad ('a':'b':_) = False
    noBad ('c':'d':_) = False
    noBad ('p':'q':_) = False
    noBad ('x':'y':_) = False
    noBad (_:xs) = noBad xs

countRealNice :: [String] -> Int
countRealNice = length . filter isNice
  where
    isNice str = hasDoublePair str && hasSandwichPair str

    hasDoublePair [] = False
    hasDoublePair [_] = False
    hasDoublePair (a:b:str)
      | [a, b] `isInfixOf` str = True
      | otherwise = hasDoublePair (b:str)

    hasSandwichPair [] = False
    hasSandwichPair [_] = False
    hasSandwichPair [_, _] = False
    hasSandwichPair (a:b:c:str)
      | a == c = True
      | otherwise = hasSandwichPair (b:c:str)

main :: IO ()
main = do
  strings <- lines <$> getContents
  print $ countNice strings
  print $ countRealNice strings
