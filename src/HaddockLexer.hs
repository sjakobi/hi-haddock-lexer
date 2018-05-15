{-# language FlexibleContexts #-}
module HaddockLexer where

import Control.Applicative
import Data.Char
import Data.Functor
import Data.Maybe
import Text.Parsec (Stream, ParsecT)
import qualified Text.Parsec as P

import FastString (mkFastString)
import Lexer (mkPStatePure, unP, ParseResult(POk), ParserFlags(..))
import Parser (parseIdentifier)
import RdrName
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

identifiersWith :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
identifiersWith identifier =
    catMaybes <$> P.many (P.try (Just <$> identifier) <|> dropDelim <|> dropUntilDelim)
  where
    dropUntilDelim = P.many1 (P.satisfy (not . isDelim)) $> Nothing
    dropDelim = identDelim $> Nothing

-- Ignores infix identifiers for now
delimitedPlausibleIdentifier :: Stream s m Char => ParsecT s u m String
delimitedPlausibleIdentifier = identDelim *> plausibleIdentifier <* identDelim

plausibleIdentifier :: Stream s m Char => ParsecT s u m String
plausibleIdentifier = do
    c <- P.satisfy isFirstIdentChar
    cs <- p
    return (c : cs)
  where
    p = do
      vs <- many (P.satisfy (\c -> isIdentChar c && c /= '\''))
      c <- P.lookAhead P.anyChar
      case c of
        '`' -> return vs
        '\'' -> P.try ((\x -> vs ++ "'" ++ x) <$> (P.char '\'' *> p)) <|> return vs
        _ -> fail "outofvalid"

identDelim :: Stream s m Char => ParsecT s u m Char
identDelim = P.satisfy isDelim

isDelim :: Char -> Bool
isDelim c = c == '\'' || c == '`'

isFirstIdentChar :: Char -> Bool
isFirstIdentChar c = (isAlpha c || c == '_' || isSymbol c) && c /= '`'

isIdentChar :: Char -> Bool
isIdentChar c = not (isSpace c) && c /= '`'

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

{-
validIdentifier :: Stream s m Char => ParsecT s u m RdrName
validIdentifier = do
  pid <- plausibleIdentifier
  case parseIdent pid of
    Just id' -> return id'
    Nothing -> P.parserFail ("Not a valid identifier: " ++ pid)
-}
