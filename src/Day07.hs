module Day07 where

import Control.Arrow ((>>>))

import Control.Applicative (some, (<|>))
import Text.Megaparsec (Parsec, parseMaybe, between)
import Text.Megaparsec.Char (noneOf, char, newline)

import qualified Data.List as List
import qualified Data.Either as Either
import Data.Void (Void)

type Parser = Parsec Void String

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
bracketedP = Bracketed <$> between (char '[') (char ']') segmentP

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
  let (unbracketed, bracketed) = Either.partitionEithers (map toEither ip)
  in any hasABBA unbracketed
     && all (hasABBA >>> not) bracketed

-- | For partitioning.
toEither :: Segment -> Either String String
toEither (Unbracketed s) = Left s
toEither (Bracketed s) = Right s

-- | Use a sliding window to determine whether there is any four-char
-- sequence ABBA.
hasABBA :: String -> Bool
hasABBA = List.tails >>> any hasABBAPrefix

hasABBAPrefix :: String -> Bool
hasABBAPrefix (a:b:b':a':_) = a /= b && a == a' && b == b'
hasABBAPrefix _ = False
