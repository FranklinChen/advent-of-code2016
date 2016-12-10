module Day03Spec where

import Day03 (triplesP, isPossibleTriangle, numPossibleTriangles, Triple)

import Test.Hspec

import Text.Megaparsec (parseMaybe)

main :: IO ()
main = hspec spec

input :: String
input = "5 10 25\n3 4 5\n"

triple1 :: Triple Integer
triple1 = (5, 10, 25)

triple2 :: Triple Integer
triple2 = (3, 4, 5)

triples :: [Triple Integer]
triples = [triple1, triple2]

spec :: Spec
spec = do
  describe "isPossibleTriangle" $ do
    it "5 10 25" $ do
      isPossibleTriangle triple1 `shouldBe` False
    it "3 4 5" $ do
      isPossibleTriangle triple2 `shouldBe` True

  describe "triplesP" $ do
    it input $ do
      parseMaybe triplesP input `shouldBe` Just triples
  describe "numPossibleTriangles" $ do
    it (show triples) $ do
      numPossibleTriangles triples `shouldBe` 1
