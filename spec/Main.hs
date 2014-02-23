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

--     it "should parse parenthesis correctly" $ do
--       "p (double(_) <$> (1..10))" `parseWith` ...
      

[
("(1..10).map(&double(_))", "(1..10).map { |a| double(a) }"),
("(1..10).map &double(_)", "(1..10).map { |a| double(a) }"), 
("(1..10).map(&double)", "(1..10).map { |a| double(a) }"),
("(1..10).map &double", "(1..10).map { |a| double(a) }"), 
("map list &blk := list.map(&blk.call)", "def map(list, &blk)\n  list.map { |a| blk.call(a) }\nend"),
("(1..10).map(&:even?)", "(1..10).map(&:even?)"),
("(1..10).map(&add(:foo, _))", ("(1..10).map { |a| add(:foo, a) }"),
("map(1..10, &double)", "map(1..10) { |a| double(a) }"),
("(1..10).map(&(incr . incr))", "(1..10).map { |a| incr(incr(a)) }")
]
