module Advent.Parser where

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String

parseWith :: Parser a -> String -> a
parseWith p s = unwrapEither . parse p "INPUT" $ s
  where
    unwrapEither (Left err) = error . parseErrorPretty' s $ err
    unwrapEither (Right value) = value
