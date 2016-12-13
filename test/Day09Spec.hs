module Day09Spec where

import Day09 (decompress)

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "decompress" $ do
    it "no markers" $ do
      decompress "ADVENT" `shouldBe` "ADVENT"
    it "repeats only B" $ do
      decompress "A(1x5)BC" `shouldBe` "ABBBBBC"
    it "3x3" $ do
      decompress "(3x3)XYZ" `shouldBe` "XYZXYZXYZ"
    it "2 sets of 2x2" $ do
      decompress "A(2x2)BCD(2x2)EFG" `shouldBe` "ABCBCDEFEFG"
    it "looks like marker but is not" $ do
      decompress "(6x1)(1x3)A" `shouldBe` "(1x3)A"
    it "more looks like marker" $ do
      decompress "X(8x2)(3x3)ABCY" `shouldBe` "X(3x3)ABC(3x3)ABCY"
