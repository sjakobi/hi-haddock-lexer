{-# language FlexibleContexts #-}
module HaddockLexer where

import Control.Applicative
import Data.Char
import Data.Functor
import Data.Maybe
import Text.Parsec (Stream, ParsecT)
import qualified Text.Parsec as P

import FastString
import HsDoc
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
  , docIdentifierString :: HsDocString
  , docIdentifierNames ::  [name]
  } deriving (Eq, Show)

data HsDoc name =
  HsDoc
    HsDocString
    [DocIdentifier name]
  deriving (Eq, Show)

lex :: String -> HsDoc RdrName
lex s = HsDoc (mkHsDocString s) (mapMaybe maybeDocIdentifier idxdPlausIds)
  where
    maybeDocIdentifier :: (Int, String, Int) -> Maybe (DocIdentifier RdrName)
    maybeDocIdentifier (ix0, pid, ix1) =
      DocIdentifier (DocIdentifierSpan ix0 ix1) (mkHsDocString pid) . (: []) <$> parseIdent pid
    idxdPlausIds =
      either (error . show)
             id
             (P.runParser (identifiersWith (delimited plausibleIdentifierWithIndices))
                          () "" s)

identifiersWith :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
identifiersWith identifier =
    catMaybes <$> P.many (P.try (Just <$> identifier) <|> handleNewline <|> dropDelim <|> dropUntilDelim)
  where
    handleNewline = do
      p0 <- P.getPosition
      _ <- P.char '\n'
      P.setPosition (P.incSourceColumn p0 1)
      return Nothing
    dropUntilDelim = P.skipMany1 (P.satisfy (\c -> not (isDelim c) && c /= '\n')) $> Nothing
    dropDelim = identDelim $> Nothing

plausibleIdentifierWithIndices :: Stream s m Char => ParsecT s u m (Int, String, Int)
plausibleIdentifierWithIndices = liftA3 (,,) getColPos plausibleIdentifier getColPos
  where
    -- not quite sure why I have to subtract 1 here
    -- Alternatively, we could use P.setPosition to start
    -- at 0.
    getColPos = (pred . P.sourceColumn) <$> P.getPosition

delimited :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
delimited = P.between identDelim identDelim

plausibleIdentifier :: Stream s m Char => ParsecT s u m String
plausibleIdentifier = plausibleOperator <|> plausibleIdentifier'

plausibleOperator :: Stream s m Char => ParsecT s u m String
plausibleOperator = P.many1 (ascSymbol <|> uniSymbol)

ascSymbol :: Stream s m Char => ParsecT s u m Char
ascSymbol = P.oneOf "!#$%&⋆+./<=>?@\\^|-~:"

uniSymbol :: Stream s m Char => ParsecT s u m Char
uniSymbol = P.satisfy (\c -> not (isAscii c) && isSymbol c)

plausibleIdentifier' :: Stream s m Char => ParsecT s u m String
plausibleIdentifier' = do
    c <- identStart
    cs <- p
    return (c : cs)
  where
    p = do
      vs <- many identLetterExcept'
      c <- P.lookAhead P.anyChar
      case c of
        '`' -> return vs
        '\'' -> P.try ((\x -> vs ++ "'" ++ x) <$> (P.char '\'' *> p)) <|> return vs
        _ -> fail "outofvalid"

identDelim :: Stream s m Char => ParsecT s u m Char
identDelim = P.satisfy isDelim

identStart :: Stream s m Char => ParsecT s u m Char
identStart = P.letter <|> P.char '_'

identLetterExcept' :: Stream s m Char => ParsecT s u m Char
identLetterExcept' = P.alphaNum <|> P.char '_'

isDelim :: Char -> Bool
isDelim c = c == '\'' || c == '`'

isFirstIdentChar :: Char -> Bool
isFirstIdentChar c = isAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

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

mkHsDocString :: String -> HsDocString
mkHsDocString = HsDocString . mkFastString

unpackHDS :: HsDocString -> String
unpackHDS (HsDocString fs) = unpackFS fs
