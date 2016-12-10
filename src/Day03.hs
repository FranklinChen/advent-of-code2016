module Day03 where

import Control.Arrow ((>>>))

import Text.Megaparsec (many, parseMaybe)
import Text.Megaparsec.Char (space, newline)
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L

main :: IO Int
main = numPossibleTriangles <$> readFile "inputs/day03.txt"

numPossibleTriangles :: String -> Int
numPossibleTriangles = parse >>> filter isPossibleTriangle >>> length

-- | Return triples if parse succeeds, else just crash for simplicity.
--
-- For robustness, parse each side into arbitrary-precision Integer.
parse :: String -> [Triple Integer]
parse s =
  case parseMaybe triplesP s of
    Just triples -> triples
    Nothing -> error "Day03.parse"

type Triple a = (a, a, a)

triplesP :: Parser [Triple Integer]
triplesP = many tripleP

tripleP :: Parser (Triple Integer)
tripleP = (,,) <$> intP <*> intP <*> intP <* newline

intP :: Parser Integer
intP = space *> L.integer

-- | In a valid triangle, the sum of any two sides must be larger than
-- the remaining side.
--
-- We only need to sum the two smallest sides.
isPossibleTriangle :: (Num a, Ord a) => Triple a -> Bool
isPossibleTriangle = sort3 >>> isPossibleOrdered

isPossibleOrdered :: (Num a, Ord a) => Triple a -> Bool
isPossibleOrdered (x1, x2, x3) = x1 + x2 > x3

-- | Sort exactly three items.
sort3 :: Ord a => Triple a -> Triple a
sort3 (x1, x2, x3) =
  if x1 <= x2 then
    if x2 <= x3 then
      (x1, x2, x3)
    else -- x3 <= x2
      if x1 <= x3 then
        (x1, x3, x2)
      else -- x3 <= x1
        (x3, x1, x2)
  else -- x2 <= x1
    if x1 <= x3 then
      (x2, x1, x3)
    else -- x3 <= x1
      if x2 <= x3 then
        (x2, x3, x1)
      else -- x3 <= x2
        (x3, x2, x1)
