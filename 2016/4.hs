import Control.Applicative
import Data.Attoparsec.Text hiding (take)
import Data.List
import Data.Ord
import Data.Char
import qualified Data.Map as Map
import qualified Data.Text as T

data Room = Room { name :: String
                 , sectorId :: Int
                 , checksum :: String
                 } deriving Show

generateChecksum :: String -> String
generateChecksum = take 5 . map fst . sortBy (comparing comparator) . histogram
  where
    comparator (c, count) = (-count, c)
    histogram = map byCt . group . sort
    byCt xs = (head xs, length xs)

valid :: Room -> Bool
valid room = checksum room == generateChecksum (name room)

decrypt :: Room -> (String, Int)
decrypt (Room name sectorId _) = (decryptName name sectorId, sectorId)
  where
    decryptName :: String -> Int -> String
    decryptName string n = map (rotate n) string

    rotate :: Int -> Char -> Char
    rotate i c = let offset = ord 'a'
                  in chr (((ord c - offset + i) `mod` 26) + offset)


-- aaaaa-bbb-z-y-x-123[abxyz]
roomParser :: Parser Room
roomParser = do
  name <- filter (/= '-') <$> many1 (letter <|> char '-')
  sectorId <- decimal
  checksum <- char '[' *> many1 letter <* char ']'

  return $! Room name sectorId checksum

main = do
  input <- parse <$> getContents
  -- print . check $ input
  print . validate $ input
  print . find $ input

 where
   parse = map (fromRight . parseOnly roomParser . T.pack) . lines
   fromRight (Right x) = x
   check = map (generateChecksum . name)
   validate = sum . map sectorId . filter valid
   find = snd . head . filter ((== "northpoleobjectstorage"). fst) . map decrypt . filter valid
