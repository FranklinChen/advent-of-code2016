module Day01 where

import Control.Arrow ((>>>))
import Data.List (foldl')

import Control.Applicative ((<|>))
import Text.Megaparsec (Parsec, sepBy, parseMaybe)
import Text.Megaparsec.Char (char, space)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

type Parser = Parsec Void String

main :: IO Int
main = do
  Just segments <- parseMaybe segmentsP <$> readFile "inputs/day01.txt"
  return (distance segments)

-- | Pipeline.
distance :: [Segment] -> Int
distance = walk >>> shortestDistance

-- | Use Megaparsec.
segmentsP :: Parser [Segment]
segmentsP = space
            *> segmentP `sepBy` (char ',' <* space)
            <* space

segmentP :: Parser Segment
segmentP = (,) <$> turnP <*> forwardP

turnP :: Parser Turn
turnP = char 'L' *> pure L
    <|> char 'R' *> pure R

forwardP :: Parser Int
forwardP = fromInteger <$> L.decimal

-- | Which way to turn, then how many blocks forward to move.
type Segment = (Turn, Int)

-- | Change in direction.
data Turn = L | R
  deriving (Show, Eq)

-- | Walk a series of segments, ending up at a point in space.
walk :: [Segment] -> Point
walk = walkFrom initState

-- | Iterate through each segment.
walkFrom :: State -> [Segment] -> Point
walkFrom state path =
  let (_, point) = foldl' step state path
  in point

-- | Step through one segment, first changing direction.
step :: State -> Segment -> State
step (direction, point) (turn, forward) =
  let direction' = newDirection direction turn
  in (direction', newPoint point (move direction' forward))

-- | Change direction when turning.
newDirection :: Direction -> Turn -> Direction
newDirection North L = West
newDirection North R = East
newDirection East L = North
newDirection East R = South
newDirection South L = East
newDirection South R = West
newDirection West L = South
newDirection West R = North

-- | New point from moving.
newPoint :: Point -> (Int, Int) -> Point
newPoint (x, y) (dx, dy) = (x + dx, y + dy)

-- | Calculate how much to move based on direction.
move :: Direction -> Int -> (Int, Int)
move North d = (0, d)
move East d = (d, 0)
move South d = (0, -d)
move West d = (-d, 0)

-- | State carried while walking.
type State = (Direction, Point)

-- | Direction facing.
data Direction = North | East | South | West

-- | Initial state.
initState :: State
initState = (North, (0, 0))

-- | 2d point in space.
type Point = (Int, Int)

-- | Shortest distance from a point to the origin.
shortestDistance :: Point -> Int
shortestDistance (x, y) = abs x + abs y
