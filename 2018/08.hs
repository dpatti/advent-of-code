{-# LANGUAGE DeriveFoldable, NamedFieldPuns #-}
import Advent hiding (count)
import Data.Data
import Data.Data.Lens (uniplate)
import Control.Applicative.Combinators (count)
import Control.Lens

data Tree a = Node a [Tree a] deriving (Show, Foldable)
type Metadata = [Int]

treeParser :: Parser (Tree Metadata)
treeParser = do
  numChildren <- integer
  numMetadataEntries <- integer
  flip Node <$> count numChildren treeParser <*> count numMetadataEntries integer

fetchFrom :: Ixed s => s -> Index s -> Maybe (IxValue s)
fetchFrom s n = s ^? ix n

valueOf :: Tree Metadata -> Int
valueOf (Node contents []) = sum contents
valueOf (Node indexes children) =
  sumOf (folded . folding (fetchFrom children . pred) . to valueOf) indexes

main :: IO ()
main = solve parse part1 part2
  where
    parse = parseWith treeParser
    part1 = sumOf (folded . folded)
    part2 = valueOf
