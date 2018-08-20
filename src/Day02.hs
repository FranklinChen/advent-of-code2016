module Day02 where

import Control.Arrow ((>>>))
import Data.List (scanl', foldl')

import Control.Applicative ((<|>), some)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (char, newline)

import Data.Char (intToDigit)
import Data.Void (Void)

type Parser = Parsec Void String

main :: IO String
main = do
  Just segments <- parseMaybe segmentsP <$> readFile "inputs/day02.txt"
  return (walk segments)

type Segment = [Move]

-- | Direction to move on keypad.
data Move = U | D | L | R
  deriving (Show, Eq)

-- | Use Megaparsec.
segmentsP :: Parser [Segment]
segmentsP = some segmentP

segmentP :: Parser Segment
segmentP = some moveP <* newline

moveP :: Parser Move
moveP = char 'U' *> pure U
    <|> char 'R' *> pure R
    <|> char 'D' *> pure D
    <|> char 'L' *> pure L

-- | Move from one button to the next.
-- | Button on keypad: 1 through 9
newtype Button = Button Int
  deriving (Show, Eq)

-- | Initial state on keypad.
initButton :: Button
initButton = Button 5

-- | Each segment generates one button.
walk :: [Segment] -> String
walk = scanl' step initButton >>> tail >>> buttonsToString

-- | From starting button to final button.
step :: Button -> Segment -> Button
step = foldl' move

-- | Move from one button to next, possibly staying in same place.
move :: Button -> Move -> Button
move b m = button (newRowColumn (rowColumn b) (diff m))

newtype Row = Row Int
  deriving (Show, Eq)

newtype Column = Column Int
  deriving (Show, Eq)

-- | 0-based row and column
rowColumn :: Button -> (Row, Column)
rowColumn (Button b) =
  ( Row ((b-1) `div` 3)
  , Column ((b-1) `mod` 3)
  )

-- | 1-based button by 0-based row and column
button :: (Row, Column) -> Button
button (Row r, Column c) = Button ((3*r + c) + 1)

-- | Note that if we "fall off", we stay at the current location.
newRowColumn :: (Row, Column) -> (Int, Int) -> (Row, Column)
newRowColumn (r, c) (dr, dc) = (newRow r dr, newColumn c dc)

newRow :: Row -> Int -> Row
newRow rr@(Row r) dr
  | r' >= 0 && r' < 3 = Row r'
  | otherwise = rr
  where r' = r + dr

newColumn :: Column -> Int -> Column
newColumn cc@(Column c) dc
  | c' >= 0 && c' < 3 = Column c'
  | otherwise = cc
  where c' = c + dc

-- | Row and column diff for a move.
diff :: Move -> (Int, Int)
diff U = (-1, 0)
diff D = (1, 0)
diff L = (0, -1)
diff R = (0, 1)

-- | Turn numeric buttons into one string.
buttonsToString :: [Button] -> String
buttonsToString = map buttonToChar

buttonToChar :: Button -> Char
buttonToChar (Button b) = intToDigit b
