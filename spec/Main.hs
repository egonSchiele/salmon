import Test.Hspec
import Common
import Types
import Utils
import Parsers

line `parseWith` parser = case parse parser "" line of
                          Left err -> error err
                          Right _ -> True

main = hspec $ do
  describe "Parsers" $ do
    it "should parse a data type" $ do
      "data Maybe val" `parseWith` classParser
