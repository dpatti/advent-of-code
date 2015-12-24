{-# LANGUAGE OverloadedStrings #-}

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Word8
import           Data.Maybe
import           Data.List

try :: BS.ByteString -> Int -> BS.ByteString
try salt n = MD5.hash (salt `BS.append` trial) 
  where trial = BSC.pack . show $ n

test :: BS.ByteString -> Bool
test = zeros . take 3 . BS.unpack
  where
    -- hack-edit for part 2 from
    --   zeros [0, 0, a] = a < 16
    zeros [0, 0, 0] = True
    zeros _ = False

search :: BS.ByteString -> Int
search secret = (+1) . fromJust . findIndex test . map (try secret) $ nats  
  where nats = 1 : map (+1) nats

trim :: BS.ByteString -> BS.ByteString
trim = BS.pack . filter (not . isSpace) . BS.unpack

main :: IO ()
main = do
  secret <- trim <$> BS.getContents
  print $ "Secret: >" `BS.append` secret `BS.append` "<"
  print . search $ secret
