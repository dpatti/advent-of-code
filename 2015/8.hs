{-

""
"abc"
"aaa\"aaa"
"\x27"

 -}
escape :: String -> String
-- pulling off leading quote
escape ('"' : str) = escape' str
  where
    escape' :: String -> String
    -- \\ -> \
    escape' ('\\' : '\\' : rest) = '\\' : escape' rest
    -- \" -> "
    escape' ('\\' : '"' : rest) = '"' : escape' rest
    -- \x<code> -> <code>
    escape' ('\\' : 'x' : a : b : rest) = 'x' : escape' rest
    -- remove trailing quote
    escape' ('"' : []) = []
    -- other
    escape' (c : cs) = c : escape' cs

encode :: String -> String
encode str = '"' : encode' str ++ "\""
  where
    encode' ('"' : str) = '\\' : '"' : encode' str
    encode' ('\\' : str) = '\\' : '\\' : encode' str
    encode' (c : str) = c : encode' str
    encode' [] = []

main :: IO ()
main = do
  input <- lines <$> getContents
  let code = total input
  let memory = total (map escape input)
  let double = total (map encode input)
  print $ code - memory
  print $ double - code
  where total = sum . map length
