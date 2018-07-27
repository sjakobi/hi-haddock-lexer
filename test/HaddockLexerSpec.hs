{-# language GeneralizedNewtypeDeriving, OverloadedStrings, StandaloneDeriving #-}
module HaddockLexerSpec (main, spec) where

import Data.Either
import Data.String
import Test.Hspec
import Text.Parsec (ParseError)
import qualified Text.Parsec as P

import HsDoc
import Module
import Name
import RdrName

import HaddockLexer (HsDoc(..), DocIdentifier(..), DocIdentifierSpan(..))
import qualified HaddockLexer as H

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "delimited plausibleIdentifier" $ do
    let dPI :: String -> Either ParseError String
        dPI = P.runParser
                (H.delimited H.plausibleIdentifier)
                () "delimited plausibleIdentifier"
    it "accepts variable names in backticks" $ do
      dPI "`foo`" `shouldParseTo` "foo"
    it "accepts variable names in single quotes" $ do
      dPI "'foo'" `shouldParseTo` "foo"
    it "accepts variable names preceded by a backtick and followed by a single quote" $ do
      dPI "`foo'" `shouldParseTo` "foo"
    it "accepts variable names containing a single quote" $ do
      dPI "`don't`" `shouldParseTo` "don't"
      dPI "'don't'" `shouldParseTo` "don't"
    it "accepts variable names ending with a single quote" $ do
      dPI "`foo'`" `shouldParseTo` "foo'"
      dPI "'foo''" `shouldParseTo` "foo'"
    it "accepts variable names ending with multiple single quotes" $ do
      dPI "'foo''''" `shouldParseTo` "foo'''"
    it "rejects strings containing whitespace" $ do
      shouldNotParse $ dPI "'foo '"
    it "accepts the longest plausible identifier before an invalid part" $ do
      dPI "'foo'o'o '" `shouldParseTo` "foo'o"
    it "accepts an operator" $ do
      dPI "'<>'" `shouldParseTo` "<>"
    it "accepts a constructor operator" $ do
      dPI "':='" `shouldParseTo` ":="
    it "rejects a mix of letters and operator symbols" $ do
      shouldNotParse $ dPI "'f$'"
  describe "identifiersWith delimitedPlausibleIdentifier" $ do
    let iWdPI :: String -> Either ParseError [String]
        iWdPI = P.runParser
                 (H.identifiersWith (H.delimited H.plausibleIdentifier))
                 ()
                 "identifiersWith (delimited plausibleIdentifier)"
    it "accepts a single identifier in backticks" $ do
      iWdPI "`foo`" `shouldParseTo` ["foo"]
    it "accepts a single identifier preceded by some text" $ do
      iWdPI "bla `foo`" `shouldParseTo` ["foo"]
    it "accepts a single identifier enclosed within some other text" $ do
      iWdPI "a`foo`b" `shouldParseTo` ["foo"]
    it "accepts a single identifier preceded by several delimiters" $ do
      iWdPI "'`'`foo`" `shouldParseTo` ["foo"]
    it "accepts an infix identifier" $ do
      iWdPI "'`foo`'" `shouldParseTo` ["foo"]
    it "parses multiple identifiers from a string" $ do
      iWdPI "`foo`, 'bar', and `baz'" `shouldParseTo` ["foo", "bar", "baz"]
    it "ignores single ticks when they don't delimit an identifier" $ do
      iWdPI "don't 'foo'" `shouldParseTo` ["foo"]
  describe "identifiersWith (delimited plausibleIdentifierWithIndices)" $ do
    let iWdPIWI :: String -> Either ParseError [(Int, String, Int)]
        iWdPIWI = P.runParser
                  (H.identifiersWith (H.delimited H.plausibleIdentifierWithIndices))
                  ()
                  "identifiersWith (delimited plausibleIdentifier)"
    it "accepts a single identifier in backticks" $ do
      iWdPIWI "`foo`" `shouldParseTo` [(1, "foo", 4)]
    it "parses multiple identifiers from a string" $ do
      iWdPIWI "`foo`, 'bar', and `baz'" `shouldParseTo` [(1, "foo", 4), (8, "bar", 11), (19, "baz", 22)]
    context "with multiline strings" $ do
      it "reports the right indices for an identifier preceded by a newline" $ do
        iWdPIWI "\n`foo`" `shouldParseTo` [(2, "foo", 5)]
      it "reports the right indices for an identifier preceded by some text" $ do
        iWdPIWI "bla\nblab`foo`" `shouldParseTo` [(9, "foo", 12)]
  describe "lex" $ do
    context "ignoring source spans" $ do
      it "detects no identifiers in the empty string" $ do
        "" `shouldLexToIdStrings` []
      it "detects no identifiers in a string with no delimiters" $ do
        "bli bla blup" `shouldLexToIdStrings` []
      it "detects no identifiers in a string with single quotes but no identifiers" $ do
        "don't hasn't" `shouldLexToIdStrings` []
      it "detects an identifier" $ do
        "'foo'" `shouldLexToIdStrings` ["foo"]
    context "looking at the whole HsDoc" $ do
      it "detects a variable identifier" $ do
        H.lex "'foo'" `shouldBe`
          HsDoc
            "'foo'"
            [DocIdentifier {docIdentifierSpan = DocIdentifierSpan 1 4
                           , docIdentifierString = "foo"
                           , docIdentifierNames = [Unqual (mkVarOcc "foo")]}]
      it "ignores an invalid pretend-identifier" $ do
        H.lex "'foo$'" `shouldBe` HsDoc "'foo$'" []

shouldLexToIdStrings :: String -> [String] -> Expectation
shouldLexToIdStrings s ids = H.unpackHDS . H.docIdentifierString <$> ids' `shouldBe` ids
  where H.HsDoc _ ids' = H.lex s

shouldParseTo :: (Eq a, Show a) => Either ParseError a -> a -> Expectation
shouldParseTo res ex = res `shouldBe` Right ex

shouldNotParse :: (Eq a, Show a) => Either ParseError a -> Expectation
shouldNotParse res = res `shouldSatisfy` isLeft

instance Show OccName where
  show = show . occNameString
instance Show ModuleName where
  show = show . moduleNameString
instance Show Name where
  show = show . nameStableString
deriving instance Show Module
deriving instance Show RdrName
deriving instance IsString HsDocString
