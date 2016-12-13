module Day09 where

import Control.Arrow ((>>>))

import Text.Megaparsec
  ( parse, parseErrorPretty
  , many, optional, satisfy, count, char, anyChar, eof, (<|>)
  )
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L

import qualified Data.Char as Char

main :: IO Int
main = (decompress >>> length) <$> readFile "inputs/day09.txt"

-- | For simplicity, just remove all whitespace up front instead of
-- interspersed with parsing.
decompress :: String -> String
decompress =
  filter (Char.isSpace >>> not)
  >>> decompressNoSpace

-- | Crash if a parse error happens, because we assume correct input.
decompressNoSpace :: String -> String
decompressNoSpace s =
  case parse startP "" s of
    Right result -> result
    Left parseError -> error $ parseErrorPretty parseError

-- | Start starte: try to consume all chars until a left paren, then
-- continue with marker and the rest.
startP :: Parser String
startP = (++) <$>
  many (satisfy (/= '('))
  <*> (expectMarkerP
       <|> eof *> pure ""
      )

-- | Expect to find a marker and continue from there.
--
-- Monadic because context-sensitive.
expectMarkerP :: Parser String
expectMarkerP = do
  (lengthToCount, numRepeats) <- markerP

  -- Use the count.
  counted <- count lengthToCount anyChar

  -- Return to start state.
  rest <- startP

  -- Prepend numRepeats copies of counted.
  return $ foldr (const (counted ++)) rest [1..numRepeats]

markerP :: Parser (Int, Int)
markerP = (,) <$>
  (char '(' *> intP <* char 'x')
  <*> intP <* char ')'

intP :: Parser Int
intP = fromInteger <$> L.integer
