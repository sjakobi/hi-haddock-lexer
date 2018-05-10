{-# language FlexibleContexts #-}
module HaddockLexer where

import Control.Applicative
import Data.Char
import Text.Parsec (Stream, ParsecT)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

import FastString (mkFastString)
import Lexer (mkPStatePure, unP, ParseResult(POk), ParserFlags(..))
import Parser (parseIdentifier)
import RdrName
import OccName
import SrcLoc (mkRealSrcLoc, unLoc)
import StringBuffer (stringToStringBuffer)
import qualified EnumSet
import Module (stringToUnitId)

data DocIdentifierSpan =
  DocIdentifierSpan
    Int
    Int
  deriving (Eq, Show)

data DocIdentifier name =
  DocIdentifier
  { docIdentifierSpan :: DocIdentifierSpan
  , docIdentifierString ::  String
  , docIdentifierNames ::  [name]
  } deriving (Eq, Show)

data HsDoc name =
  HsDoc
    String
    [DocIdentifier name]
  deriving (Eq, Show)

lex :: String -> HsDoc RdrName
lex s = HsDoc s []

-- adapted from haddock-api
parseIdent :: String -> Maybe RdrName
parseIdent str0 =
  let pflags = ParserFlags EnumSet.empty EnumSet.empty (stringToUnitId "unknown package") 0
      buffer = stringToStringBuffer str0
      realSrcLc = mkRealSrcLoc (mkFastString "<unknown file>") 0 0
      pstate = mkPStatePure pflags buffer realSrcLc
  in case unP parseIdentifier pstate of
    POk _ name -> Just (unLoc name)
    _ -> Nothing

isIdentChar :: Char -> Bool
isIdentChar c = not (isSpace c) && c /= '`'

isFirstIdentChar :: Char -> Bool
isFirstIdentChar c = isAlpha c || c == '_' || isSymbol c

plausibleIdentifier :: Stream s m Char => ParsecT s u m String
plausibleIdentifier =
  liftA2
    (:)
    (P.satisfy isFirstIdentChar)
    (P.many (P.satisfy isIdentChar))

startDelim :: Stream s m Char => ParsecT s u m Char
startDelim = P.satisfy (\c -> c == '\'' || c == '`')

endDelim :: Stream s m Char => ParsecT s u m Char
endDelim = P.char '`' <|> (singleQuote <* P.notFollowedBy singleQuote)
  where singleQuote = P.char '\''

-- Ignores infix identifiers for now
delimitedPlausibleIdentifier :: Stream s m Char => ParsecT s u m String
delimitedPlausibleIdentifier = startDelim *> plausibleIdentifier <* endDelim

{-
validIdentifier :: Stream s m Char => ParsecT s u m RdrName
validIdentifier = do
  pid <- plausibleIdentifier
  case parseIdent pid of
    Just id' -> return id'
    Nothing -> P.parserFail ("Not a valid identifier: " ++ pid)
-}
