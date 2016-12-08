module Day01

where

import Control.Arrow ((>>>))
import Data.List (foldl')

main :: IO Int
main = distance <$> readFile "inputs/day01.txt"

-- | Pipeline.
distance :: String -> Int
distance = parse >>> walk >>> shortestDistance

parse :: String -> [Segment]
parse = error "parse"

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

step :: State -> Segment -> State
step = error "step"

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
