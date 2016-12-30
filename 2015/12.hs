{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString as BS
import Data.HashMap.Strict (elems)
import Data.Maybe
import Data.Scientific

count :: Value -> Int
count a = if isRed a then 0 else count' a
  where
    isRed (Object a) = elem (String "red") . elems $ a
    isRed _ = False

    count' (Number a) = fromJust. toBoundedInteger $ a
    count' (Array a) = sum . fmap count $ a
    count' (Object a) = sum . elems . fmap count $ a
    -- Everything else
    count' _ = 0

main :: IO ()
main = do
  input <- BS.getContents

  print . count . fromJust . decodeStrict $ input
