module Day02Spec where

import Day02 (run, parse, step, move, Move (U, D, L, R), Button (..))

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Day02" $ do
    let input = "ULL\nRRDDD\nLURDL\nUUUUD\n"

    describe "run" $ do
      it input $ do
        run input `shouldBe` "1985"
    describe "parse" $ do
      it input $ do
        parse input `shouldBe` [ [U, L, L]
                               , [R, R, D, D, D]
                               , [L, U, R, D, L]
                               , [U, U, U, U, D]
                               ]
    describe "step" $ do
      it "5 moving ULL" $ do
        step (Button 5) [U, L, L] `shouldBe` (Button 1)
      it "1 moving RRDDD" $ do
        step (Button 1) [R, R, D, D, D] `shouldBe` (Button 9)
    describe "move" $ do
      it "move 1" $ do
        move (Button 1) U `shouldBe` (Button 1)
        move (Button 1) D `shouldBe` (Button 4)
        move (Button 1) L `shouldBe` (Button 1)
        move (Button 1) R `shouldBe` (Button 2)
      it "move 8" $ do
        move (Button 8) U `shouldBe` (Button 5)
        move (Button 8) D `shouldBe` (Button 8)
        move (Button 8) L `shouldBe` (Button 7)
        move (Button 8) R `shouldBe` (Button 9)
