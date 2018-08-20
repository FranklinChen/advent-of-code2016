module Day08 where

import Control.Arrow ((>>>))

import Control.Applicative (some, (<|>))
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (char, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.List as List

import qualified Data.Matrix.Unboxed as M
import Data.Matrix.Unboxed (Matrix, (!))
import Data.Void (Void)

type Parser = Parsec Void String

main :: IO Int
main = do
   Just instructions <- parseMaybe instructionsP <$> readFile "inputs/day08.txt"
   return (numLitAfterRun instructions)

instructionsP :: Parser [Instruction]
instructionsP = some (instructionP <* newline)

instructionP :: Parser Instruction
instructionP = rectP
  <|> rotateRowP
  <|> rotateColumnP

rectP :: Parser Instruction
rectP = Rect <$>
        (string "rect " *> intP <* char 'x') <*> intP

rotateRowP :: Parser Instruction
rotateRowP = RotateRow <$>
             (string "rotate row y=" *> intP <* string " by ") <*> intP

rotateColumnP :: Parser Instruction
rotateColumnP = RotateColumn <$>
                (string "rotate column x=" *> intP <* string " by ") <*> intP

-- | Assume small enough number to fit into Int.
intP :: Parser Int
intP = fromInteger <$> L.decimal

numLitAfterRun :: [Instruction] -> Int
numLitAfterRun =
  run >>> numLit

data Instruction =
  Rect Int Int            -- ^ wide, tall from left
  | RotateRow Int Int     -- ^ 0-based index, amount
  | RotateColumn Int Int  -- ^ 0-based index, amount
  deriving (Show, Eq)

-- | Matrix of off/on.
--
-- For efficiency we could use a mutable matrix, but for clarity here,
-- use an immutable one.
type Display = Matrix Bool

-- | 50x6
initDisplay :: Display
initDisplay = M.fromLists
  $ replicate 6
  $ replicate 50 False

-- | Count how many are on.
numLit :: Display -> Int
numLit = M.toList >>> filter (== True) >>> length

run :: [Instruction] -> Display
run = List.foldl' step initDisplay

step :: Display -> Instruction -> Display
step m (Rect wide tall) = rect wide tall m
step m (RotateRow index amount) = rotateRow index amount m
step m (RotateColumn index amount) = rotateColumn index amount m

-- | Fill upper left with True.
rect :: Int -> Int -> Display -> Display
rect wide tall =
  M.imap (\(i, j) x ->
             if i < tall && j < wide then
               True
             else
               x
         )

-- | Rotate row with wraparound.
rotateRow :: Int -> Int -> Display -> Display
rotateRow index amount m =
  let cols = M.cols m
  in M.imap (
    \(i, j) x ->
      if i == index then
        m ! (i, (j - amount) `mod` cols)
      else
        x
    ) m

-- | Rotate column with wraparound.
rotateColumn :: Int -> Int -> Display -> Display
rotateColumn index amount m =
  let rows = M.rows m
  in M.imap (
    \(i, j) x ->
      if j == index then
        m ! ((i - amount) `mod` rows, j)
      else
        x
    ) m
