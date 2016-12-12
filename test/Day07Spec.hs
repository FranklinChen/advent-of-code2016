module Day07Spec where

import Day07 (ipP, supportsTLS)

import Test.Hspec

import Data.Foldable (for_)
import Text.Megaparsec (parseMaybe)

main :: IO ()
main = hspec spec

data Ex =
  Ex { string :: String
     , expected :: Bool
     }

examples :: [Ex]
examples =
  [ Ex "abba[mnop]qrst" True
  , Ex "abcd[bddb]xyyx" False
  , Ex "aaaa[qwer]tyui" False
  , Ex "ioxxoj[asdfgh]zxcvbn" True
  ]

spec :: Spec
spec = do
  describe "supportsTLS" $ do
    for_ examples $ \ex ->
      it (string ex) $ do
        let Just ip = parseMaybe ipP (string ex)
        supportsTLS ip `shouldBe` (expected ex)
