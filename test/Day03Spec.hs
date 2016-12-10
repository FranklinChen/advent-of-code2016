module Day03Spec where

import Day03 (parse, isPossibleTriangle, numPossibleTriangles)

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day03" $ do
    let triple1 = (5, 10, 25)
    let triple2 = (3, 4, 5)
    describe "isPossibleTriangle" $ do
      it "5 10 25" $ do
        isPossibleTriangle triple1 `shouldBe` False
      it "3 4 5" $ do
        isPossibleTriangle triple2 `shouldBe` True

    let triples = [triple1, triple2]
    let input = "5 10 25\n3 4 5\n"

    describe "parse" $ do
      it input $ do
        parse input `shouldBe` triples
    describe "numPossibleTriangles" $ do
      it input $ do
        numPossibleTriangles input `shouldBe` 1
