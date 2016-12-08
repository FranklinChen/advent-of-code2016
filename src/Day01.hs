module Day01

where

distance :: String -> Int
distance = error "distance"

main :: IO Int
main = distance <$> readFile "inputs/day01.txt"
