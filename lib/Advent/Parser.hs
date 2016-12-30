module Advent.Parser where

import Advent.Either
import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.String

parseWith :: Parser a -> String -> a
parseWith p = unwrapEither . parse p "INPUT"
  where
    unwrapEither (Left err) = error . parseErrorPretty $ err
    unwrapEither (Right value) = value
