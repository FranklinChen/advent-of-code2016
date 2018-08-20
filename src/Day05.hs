{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Control.Arrow ((>>>))

import qualified Crypto.Hash as Hash

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as Builder
import Data.Maybe (mapMaybe)
import qualified Data.List as List

main :: IO String
main = return (password "ojvtpuvg")

-- | Look for hex hashes starting with 5 zeros, and collect the 6th
-- character for the first 8 instances.
password :: B.ByteString -> String
password =
  ids
  >>> mapMaybe (hashlazy >>> show >>> List.stripPrefix "00000")
  >>> map head
  >>> take 8

hashlazy :: L.ByteString -> Hash.Digest Hash.MD5
hashlazy = Hash.hashlazy

-- | Generate all possible ids in order.
ids :: B.ByteString -> [L.ByteString]
ids doorId = map (Builder.intDec
                  >>> (Builder.byteString doorId <>)
                  >>> Builder.toLazyByteString) [0..]
