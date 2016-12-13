module Day04 where

import Control.Arrow ((>>>))

import Text.Megaparsec (parseMaybe, many, some, between)
import Text.Megaparsec.Char (char, lowerChar, newline)
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L

import qualified Data.IntMap.Strict as Map
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Ord as Ord

main :: IO Integer
main = do
  Just rooms <- parseMaybe roomsP <$> readFile "inputs/day04.txt"
  return (run rooms)

-- | Return sum of the sector IDs of the real rooms.
run :: [Room] -> Integer
run = filter isRealRoom >>> map sector >>> sum

-- | Fully parsed room.
data Room =
  Room { name :: [String] -- ^ lowercase letters separated by dashes
       , sector :: Integer
       , checksum :: String
       }
  deriving (Show, Eq)

roomsP :: Parser [Room]
roomsP = many (roomP <* newline)

roomP :: Parser Room
roomP = Room <$>
        some (lowerChars <* char '-')
        <*> L.integer
        <*> between (char '[') (char ']') lowerChars

lowerChars :: Parser String
lowerChars = some lowerChar

isRealRoom :: Room -> Bool
isRealRoom room =
  calculateChecksum room == checksum room

calculateChecksum :: Room -> String
calculateChecksum =
  name
  >>> concat                               -- all the letters
  >>> map (\c -> (Char.ord c, 1))          -- convert letter Int, count 1
  >>> Map.fromListWith (+)                 -- sum up counts for each
  >>> Map.toList                           -- [(c, count), ...]
  >>> List.sortOn ourOrder                 -- sort by most common letters
  >>> take 5                               -- 5 most common
  >>> map fst                              -- [c, ...]
  >>> map Char.chr                         -- get back chars

-- | Order by decreasing count but ascending letter.
ourOrder :: (Map.Key, Integer) -> (Ord.Down Integer, Map.Key)
ourOrder (c, count) = (Ord.Down count, c)
