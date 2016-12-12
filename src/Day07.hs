module Day07 where

import Control.Arrow ((>>>))

import Text.Megaparsec (parseMaybe, some, (<|>))
import Text.Megaparsec.Char (noneOf, char, newline)
import Text.Megaparsec.String (Parser)

import qualified Data.List as List

main :: IO Int
main = do
   Just ips <- parseMaybe ipsP <$> readFile "inputs/day07.txt"
   return (howManySupportTLS ips)

ipsP :: Parser [IP]
ipsP = some (ipP <* newline)

ipP :: Parser IP
ipP = some (unbracketedP <|> bracketedP)

unbracketedP :: Parser Segment
unbracketedP = Unbracketed <$> segmentP

bracketedP :: Parser Segment
bracketedP = Bracketed <$> (char '[' *> segmentP <* char ']')

segmentP :: Parser String
segmentP = some (noneOf "[]\n")

howManySupportTLS :: [IP] -> Int
howManySupportTLS =
  filter supportsTLS
  >>> length

type IP = [Segment]

data Segment = Unbracketed String
  | Bracketed String
  deriving (Show, Eq)

-- | Must be at least one unbracketed ABBA but no bracketed ABBA.
supportsTLS :: IP -> Bool
supportsTLS ip =
  let (unbracketed, bracketed) = List.partition isUnbracketed ip
  in any hasABBA (map unwrap unbracketed)
     && all (hasABBA >>> not) (map unwrap bracketed)

isUnbracketed :: Segment -> Bool
isUnbracketed (Unbracketed _) = True
isUnbracketed _ = False

unwrap :: Segment -> String
unwrap (Unbracketed s) = s
unwrap (Bracketed s) = s

-- | Use a sliding window to determine whether there is any four-char
-- sequence ABBA.
hasABBA :: String -> Bool
hasABBA = List.tails >>> any hasABBAPrefix

hasABBAPrefix :: String -> Bool
hasABBAPrefix (a:b:b':a':_) = a /= b && a == a' && b == b'
hasABBAPrefix _ = False
