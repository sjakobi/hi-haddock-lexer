module LexerSpec (main, spec) where

import Test.Hspec

import qualified HaddockLexer

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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

shouldLexToIdStrings :: String -> [String] -> Expectation
shouldLexToIdStrings s ids = HaddockLexer.docIdentifierString <$> ids' `shouldBe` ids
  where HaddockLexer.HsDoc _ ids' = HaddockLexer.lex s
