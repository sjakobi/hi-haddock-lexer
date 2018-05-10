{-# language StandaloneDeriving #-}
module HaddockLexer where

import DynFlags (DynFlags)
import FastString (mkFastString)
import Lexer (mkPStatePure, unP, ParseResult(POk), ParserFlags(..))
import Parser (parseIdentifier)
import RdrName
import OccName
import SrcLoc (mkRealSrcLoc, unLoc)
import StringBuffer (stringToStringBuffer)
import EnumSet (empty)
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
  let pflags = ParserFlags empty empty (stringToUnitId "unknown package") 0
      buffer = stringToStringBuffer str0
      realSrcLc = mkRealSrcLoc (mkFastString "<unknown file>") 0 0
      pstate = mkPStatePure pflags buffer realSrcLc
  in case unP parseIdentifier pstate of
    POk _ name -> Just (unLoc name)
    _ -> Nothing
