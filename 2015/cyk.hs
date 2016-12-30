{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text hiding (take, takeWhile)
import qualified Data.Text as T

data RHS = Terminal String
         | Sequence String String
type Production = (String, RHS)
type Grammar = [Production]

seqProd :: String -> String -> String -> Production
seqProd a b1 b2 = (a, Sequence b1 b2)

termProd :: String -> String -> Production
termProd a b = (a, Terminal b)

grammar = [ seqProd "S" "NP" "VP"
          , seqProd "VP" "VP" "PP"
          , seqProd "VP" "V" "NP"
          , seqProd "PP" "P" "NP"
          , seqProd "NP" "Det" "N"
          , termProd "NP" "she"
          , termProd "VP" "eats"
          , termProd "V" "eats"
          , termProd "P" "with"
          , termProd "N" "fish"
          , termProd "N" "fork"
          , termProd "Det" "a"
          ]

target = map (Terminal . T.pack) . words $ "she eats a fish with a fork"

parse :: Grammar -> Sentence -> Tree

main = print . parse grammar $ target
