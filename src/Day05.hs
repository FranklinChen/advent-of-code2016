{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Control.Arrow ((>>>))

import qualified Crypto.Hash.MD5 as MD5

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as Builder
import Data.Semigroup ((<>))
import Data.Maybe (mapMaybe)

main :: IO B.ByteString
main = return (password "ojvtpuvg")

-- | Look for hex hashes starting with 5 zeros, and collect the 6th
-- character for the first 8 instances.
password :: B.ByteString -> B.ByteString
password =
  ids
  >>> mapMaybe (hexHash >>> L.stripPrefix "00000")
  >>> map L.head
  >>> take 8
  >>> B.pack

hexHash :: L.ByteString -> L.ByteString
hexHash = MD5.hashlazy
          >>> Builder.byteStringHex
          >>> Builder.toLazyByteString

-- | Generate all possible ids in order.
ids :: B.ByteString -> [L.ByteString]
ids doorId = map (Builder.intDec
                  >>> (Builder.byteString doorId <>)
                  >>> Builder.toLazyByteString) [0..]
