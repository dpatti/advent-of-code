{-# LANGUAGE PackageImports #-}

module Advent.Hash where

import qualified "cryptonite" Crypto.Hash as Hash
import qualified Data.ByteString.Char8 as BS

md5 :: String -> String
md5 = show . Hash.hashWith Hash.MD5 . BS.pack
