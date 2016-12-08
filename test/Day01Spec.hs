module Day01Spec where

import Day01 (distance, parse, walk, shortestDistance, Turn(L, R))

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day01" $ do
    describe "Given examples" $ do
      it "Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away" $ do
        distance "R2, L3" `shouldBe` 5
      it "R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away" $ do
        distance "R2, R2, R2" `shouldBe` 2
      it "R5, L5, R5, R3 leaves you 12 blocks away" $ do
        distance "R5, L5, R5, R3" `shouldBe` 12
    describe "parse" $ do
      it "R5, L5, R5, R3" $ do
        parse "R5, L5, R5, R3" `shouldBe` [(R, 5), (L, 5), (R, 5), (R, 3)]
    describe "walk" $ do
      it "R2, L3" $ do
        walk [(R, 2), (L, 3)] `shouldBe` (2, 3)
      it "R2, R2, R2" $ do
        walk [(R, 2), (R, 2), (R, 2)] `shouldBe` (0, -2)
    describe "shortestDistance" $ do
      it "(2, 3)" $ do
        shortestDistance (2, 3) `shouldBe` 5
      it "(-2, 3)" $ do
        shortestDistance (-2, 3) `shouldBe` 5
