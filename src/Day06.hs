module Day06 where

import Control.Arrow ((>>>))

import qualified Data.List as List
import qualified Data.IntMap.Strict as Map
import qualified Data.Char as Char
import qualified Data.Ord as Ord

main :: IO String
main = message <$> readFile "inputs/day06.txt"

message :: String -> String
message =
  lines
  >>> List.transpose
  >>> map mostCommon

mostCommon :: [Char] -> Char
mostCommon =
  map (\c -> (Char.ord c, 1)) -- char with count
  >>> Map.fromListWith (+)    -- sum counts for each char
  >>> Map.toList              -- [(ord of c, count), ...]
  >>> List.sortOn ourOrder    -- sort
  >>> head                    -- (ord of c, count)
  >>> fst                     -- ord of c
  >>> Char.chr                -- c

-- | Order by decreasing count but ascending letter.
ourOrder :: (Map.Key, Int) -> (Ord.Down Int, Map.Key)
ourOrder (c, count) = (Ord.Down count, c)
