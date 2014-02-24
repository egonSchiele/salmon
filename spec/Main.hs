{-# LANGUAGE RankNTypes, FlexibleContexts #-}

import Test.Hspec
import Common
import Types
import Utils
import Parsers
import System.IO.Unsafe

type Label = String
type RubyString = String
type Expected = String

checkParser :: RubyParser -> RubyString -> Expected -> Bool
checkParser parser line expected = case parse parser "" line of
                               Left err -> error (show err)
                               Right actual -> (toRuby actual) == expected

check :: String -> String -> Bool
check line expected = if actual == expected
                        then True
                        else error ("expected: " ++ expected ++ "\ngot: " ++ actual)
    where func = parseRuby (Unresolved line)
          actual = toRuby . fst . unsafePerformIO $ runStateT func defaultState

bulkCheck checks = forM_ checks $ \(label, ruby, expected) -> do
                      it label $ do
                        check ruby expected

main = hspec $ do
  describe "Parsers" $ do
    bulkCheck $
      [("data types", "data Maybe val", "Maybe = Struct.new(:val)"),
       ("function", "add a b := a + b", "def add(a, b)\n  a+b\nend")
      ]

--     it "should parse parenthesis correctly" $ do
--       "p (double(_) <$> (1..10))" `parseWith` ...
      
-- parseFunction 

-- parseAtom "a" should be Right

-- ("incr := add(1, _)", "def incr(a)\n  add(1, a)\nend")

-- ("incr . incr <$> (1..10)", "(1..10).map { |a| incr(incr(a)) }")

-- "parse := (JSON.parse . File.read) $ file", "def parse(file)\n  JSON.parse(File.read(file))\nend"


-- [


-- ("(1..10).map(&double(_))", "(1..10).map { |a| double(a) }"),
-- ("(1..10).map &double(_)", "(1..10).map { |a| double(a) }"), 
-- ("(1..10).map(&double)", "(1..10).map { |a| double(a) }"),
-- ("(1..10).map &double", "(1..10).map { |a| double(a) }"), 
-- ("map list &blk := list.map(&blk.call)", "def map(list, &blk)\n  list.map { |a| blk.call(a) }\nend"),
-- ("(1..10).map(&:even?)", "(1..10).map(&:even?)"),
-- ("(1..10).map(&add(:foo, _))", ("(1..10).map { |a| add(:foo, a) }"),
-- ("map(1..10, &double)", "map(1..10) { |a| double(a) }"),
-- ("(1..10).map(&(incr . incr))", "(1..10).map { |a| incr(incr(a)) }")
-- ]
