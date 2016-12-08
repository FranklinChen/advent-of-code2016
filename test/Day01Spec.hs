module Day01Spec where

import qualified Day01

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day01" $ do
    it "Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away" $ do
      Day01.distance "R2, L3" `shouldBe` 5
    it "R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away" $ do
      Day01.distance "R2, R2, R2" `shouldBe` 2
    it "R5, L5, R5, R3 leaves you 12 blocks away" $ do
      Day01.distance "R5, L5, R5, R3" `shouldBe` 12
