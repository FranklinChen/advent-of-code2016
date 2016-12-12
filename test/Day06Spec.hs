{-# LANGUAGE OverloadedStrings #-}

module Day06Spec where

import Day06 (message)

import Test.Hspec

main :: IO ()
main = hspec spec

input :: String
input = "eedadn\n\
\drvtee\n\
\eandsr\n\
\raavrd\n\
\atevrs\n\
\tsrnev\n\
\sdttsa\n\
\rasrtv\n\
\nssdts\n\
\ntnada\n\
\svetve\n\
\tesnvt\n\
\vntsnd\n\
\vrdear\n\
\dvrsen\n\
\enarar\n"

output :: String
output = "easter"

spec :: Spec
spec = do
  describe "message" $ do
    it input $ do
      message input `shouldBe` output
