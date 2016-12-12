{-# LANGUAGE OverloadedStrings #-}

module Day05Spec where

import Day05 (password)

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "password" $ do
    let example1 = "abc"
    it (show example1) $ do
      password example1 `shouldBe` "18f47a30"
