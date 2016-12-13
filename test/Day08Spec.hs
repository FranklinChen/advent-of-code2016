module Day08Spec where

import Day08 (step, Display, Instruction (Rect, RotateRow, RotateColumn))

import Test.Hspec

import Control.Arrow ((>>>))
import qualified Data.Matrix.Unboxed as M

main :: IO ()
main = hspec spec

-- | For visual convenience.
fromPretty :: [[Char]] -> Display
fromPretty = map (map (== '#'))
  >>> M.fromLists

spec :: Spec
spec = do
  describe "step" $ do
    let display1 = fromPretty [ "......."
                              , "......."
                              , "......."
                              ]
    let display2 = fromPretty [ "###...."
                              , "###...."
                              , "......."
                              ]
    it "Rect 3 2" $ do
      step display1 (Rect 3 2) `shouldBe` display2

    let display3 = fromPretty [ "#.#...."
                              , "###...."
                              , ".#....."
                              ]
    it "RotateColumn 1 1" $ do
      step display2 (RotateColumn 1 1) `shouldBe` display3

    let display4 = fromPretty [ "....#.#"
                              , "###...."
                              , ".#....."
                              ]
    it "RotateRow 0 4" $ do
      step display3 (RotateRow 0 4) `shouldBe` display4

    let display5 = fromPretty [ ".#..#.#"
                              , "#.#...."
                              , ".#....."
                              ]
    it "RotateColumn 1 1 again" $ do
      step display4 (RotateColumn 1 1) `shouldBe` display5
