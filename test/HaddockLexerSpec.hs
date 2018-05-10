module HaddockLexerSpec (main, spec) where

import Data.Either
import Test.Hspec
import Text.Parsec (ParseError)
import qualified Text.Parsec as P

import qualified HaddockLexer

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "plausibleIdentifier" $ do
    let pI = P.runParser (HaddockLexer.plausibleIdentifier <* P.eof) () "plausibleIdentifier"

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
  describe "delimitedPlausibleIdentifier" $ do
    let dPI = P.runParser (HaddockLexer.delimitedPlausibleIdentifier <* P.eof) () "delimitedPlausibleIdentifier"
    it "accepts variable names in backticks" $ do
      dPI "`foo`" `shouldParseTo` "foo"
    it "accepts variable names in single quotes" $ do
      dPI "'foo'" `shouldParseTo` "foo"
  describe "lex" $ do
    context "ignoring source spans" $ do
      it "detects no identifiers in the empty string" $ do
        "" `shouldLexToIdStrings` []
      it "detects no identifiers in a string with no delimiters" $ do
        "bli bla blup" `shouldLexToIdStrings` []
      it "detects no identifiers in a string with single quotes but no identifiers" $ do
        "don't hasn't" `shouldLexToIdStrings` []
{-
      it "detects an identifier" $ do
        "'foo'" `shouldLexToIdStrings` ["foo"]
-}

shouldLexToIdStrings :: String -> [String] -> Expectation
shouldLexToIdStrings s ids = HaddockLexer.docIdentifierString <$> ids' `shouldBe` ids
  where HaddockLexer.HsDoc _ ids' = HaddockLexer.lex s

shouldParseTo :: (Eq a, Show a) => Either ParseError a -> a -> Expectation
shouldParseTo res ex = res `shouldBe` Right ex

shouldNotParse :: (Eq a, Show a) => Either ParseError a -> Expectation
shouldNotParse res = res `shouldSatisfy` isLeft
