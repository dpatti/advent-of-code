module Advent.Parser where

import Data.Char
import Data.Functor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

anyChar :: Parser Char
anyChar = anySingle

notChar :: Char -> Parser Char
notChar = anySingleBut

satisfy :: (Char -> Bool) -> Parser Char
satisfy = Text.Megaparsec.satisfy

parseWith :: Parser a -> String -> a
parseWith p = unwrapEither . parse p "INPUT"
  where
    unwrapEither (Left err) = error . errorBundlePretty $ err
    unwrapEither (Right value) = value

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

word :: Parser String
word = lexeme (some (Text.Megaparsec.satisfy (not . isSpace)))

integer :: Parser Int
integer = lexeme (L.signed space L.decimal)

symbol :: String -> Parser String
symbol = L.symbol space

($>) :: Functor f => f a -> b -> f b
($>) = (Data.Functor.$>)
