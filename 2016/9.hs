import Control.Applicative
import Data.Maybe
import Data.Attoparsec.Text
import qualified Data.Text as T
import Debug.Trace

fromRight (Left e) = error e
fromRight (Right v) = v

parseCompressed :: T.Text -> String
parseCompressed = (fromRight .) <$> parseOnly $ compressionParser
  where
    end = endOfLine <|> endOfInput
    compressionParser = (end *> return "") <|> do
      text <- manyTill anyChar (char '(' *> return () <|> end)
      repeated <- option "" $ do
        -- char '('
        charCount <- decimal
        char 'x'
        repeatCount <- decimal
        char ')'
        repeated <- T.unpack <$> Data.Attoparsec.Text.take charCount

        return . concat . replicate repeatCount $ repeated

      ((text ++ repeated) ++) <$> compressionParser

parseCompressedLength :: T.Text -> Int
parseCompressedLength = (fromRight .) <$> parseOnly $ parser
  where
    end = endOfLine <|> endOfInput
    parser = (end *> return 0) <|> do
      text <- manyTill anyChar (char '(' *> return () <|> end)
      repeated <- option 0 $ do
        -- char '('
        charCount <- decimal
        char 'x'
        repeatCount <- decimal
        char ')'
        repeated <- Data.Attoparsec.Text.take charCount

        return $ repeatCount * parseCompressedLength repeated

      ((length text + repeated) +) <$> parser

main = do
  input <- parse <$> getContents
  print . solve1 $ input
  print . solve2 $ input

 where
   parse = T.pack
   solve1 = length . parseCompressed
   solve2 = parseCompressedLength
