{-# language StandaloneDeriving #-}
module HaddockLexerSpec (main, spec) where

import Data.Either
import Test.Hspec
import Text.Parsec (ParseError)
import qualified Text.Parsec as P

import Module
import Name
import RdrName

import HaddockLexer (HsDoc(..), DocIdentifier(..), DocIdentifierSpan(..))
import qualified HaddockLexer

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
{-
  describe "plausibleIdentifier" $ do
    let pI = P.runParser (HaddockLexer.plausibleIdentifier) () "plausibleIdentifier"

    it "accepts variable names" $ do
      pI "foo" `shouldParseTo` "foo"
    it "accepts operators" $ do
      pI "<$>" `shouldParseTo` "<$>"
    it "accepts variable names including single ticks" $ do
      pI "don't'" `shouldParseTo` "don't'"
    it "rejects whitespace" $ do
      shouldNotParse $ pI " "
    it "rejects whitespace after some valid letters" $ do
      shouldNotParse $ pI "foo "
    it "rejects pretend-identifiers starting with a digit" $ do
      shouldNotParse $ pI "3foo"
    it "rejects the empty string" $ do
      shouldNotParse $ pI ""
    it "accepts identifiers starting with an underscore" $ do
      pI "_foo" `shouldParseTo` "_foo"
-}
  describe "delimitedPlausibleIdentifier" $ do
    let dPI = P.runParser (HaddockLexer.delimitedPlausibleIdentifier) () "delimitedPlausibleIdentifier"
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
  describe "identifiersWith delimitedPlausibleIdentifier" $ do
    let iWdPI = P.runParser
                 (HaddockLexer.identifiersWith HaddockLexer.delimitedPlausibleIdentifier)
                 ()
                 "identifiersWith delimitedPlausibleIdentifier"
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
  describe "identifiersWith delimitedPlausibleIdentifierWithIndices" $ do
    let iWdPIWI = P.runParser
                  (HaddockLexer.identifiersWith HaddockLexer.delimitedPlausibleIdentifierWithIndices)
                  ()
                  "identifiersWith delimitedPlausibleIdentifier"
    it "accepts a single identifier in backticks" $ do
      iWdPIWI "`foo`" `shouldParseTo` [(1, "foo", 4)]
    it "parses multiple identifiers from a string" $ do
      iWdPIWI "`foo`, 'bar', and `baz'" `shouldParseTo` [(1, "foo", 4), (8, "bar", 11), (19, "baz", 22)]
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
        HaddockLexer.lex "'foo'" `shouldBe`
          HsDoc
            "'foo'"
            [DocIdentifier {docIdentifierSpan = DocIdentifierSpan 1 4
                           , docIdentifierString = "foo"
                           , docIdentifierNames = [Unqual (mkVarOcc "foo")]}]
      it "ignores an invalid pretend-identifier" $ do
        HaddockLexer.lex "'foo$'" `shouldBe` HsDoc "'foo$'" []

shouldLexToIdStrings :: String -> [String] -> Expectation
shouldLexToIdStrings s ids = HaddockLexer.docIdentifierString <$> ids' `shouldBe` ids
  where HaddockLexer.HsDoc _ ids' = HaddockLexer.lex s

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
