module Day04Spec where

import Day04 (roomP, Room (..), calculateChecksum, isRealRoom)

import Test.Hspec

import Data.Foldable (for_)
import Text.Megaparsec (parseMaybe)

main :: IO ()
main = hspec spec

-- | Example for test.
data Ex =
  Ex { unparsed :: String
     , room :: Room
     , actualChecksum :: String
     , isReal :: Bool
     }

examples :: [Ex]
examples =
  [ Ex "aaaaa-bbb-z-y-x-123[abxyz]"
    (Room ["aaaaa", "bbb", "z", "y", "x"] 123 "abxyz")
    "abxyz"
    True
  , Ex "a-b-c-d-e-f-g-h-987[abcde]"
    (Room ["a", "b", "c", "d", "e", "f", "g", "h"] 987 "abcde")
    "abcde"
    True
  , Ex "not-a-real-room-404[oarel]"
    (Room ["not", "a", "real", "room"] 404 "oarel")
    "oarel"
    True
  , Ex "totally-real-room-200[decoy]"
    (Room ["totally", "real", "room"] 200 "decoy")
    "loart"
    False
  ]

spec :: Spec
spec =
  describe "Day04" $ do
    describe "parse" $ do
      for_ examples $ \e ->
        it (unparsed e) $ do
          parseMaybe roomP (unparsed e) `shouldBe` Just (room e)
    describe "calculateChecksum" $ do
      for_ examples $ \e ->
        it (unparsed e) $ do
          calculateChecksum (room e) `shouldBe` actualChecksum e
    describe "isRealRoom" $ do
      for_ examples $ \e ->
        it (unparsed e) $ do
          isRealRoom (room e) `shouldBe` isReal e
