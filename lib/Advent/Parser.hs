module Advent.Parser where

import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parseWith :: Parser a -> String -> a
parseWith p s = unwrapEither . parse p "INPUT" $ s
  where
    unwrapEither (Left err) = error . parseErrorPretty' s $ err
    unwrapEither (Right value) = value

word :: Parser String
word = some (satisfy (not . isSpace)) <* space1

signedInt :: Parser Int
signedInt = L.signed space L.decimal

symbol :: String -> Parser String
symbol = L.symbol space

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
