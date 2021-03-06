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
                               Right actual -> if (toRuby actual) == expected then True else error (toRuby actual)

check = checkWithState defaultState

checkWithState :: CodeState -> String -> String -> Bool
checkWithState state line expected = if actual == expected
                        then True
                        else error ("expected: " ++ expected ++ "\ngot: " ++ actual)
    where func = parseRuby (Unresolved line)
          actual = toRuby . fst . unsafePerformIO $ runStateT func state

bulkCheck checks = forM_ checks $ \(label, ruby, expected) -> do
                      it label $ do
                        check ruby expected

mapFunc :: String -> StateT CodeState IO Ruby
mapFunc line = parseRuby (Unresolved line)

combine acc line = acc >> parseRuby (Unresolved line)

-- good for checking things that take place over multiple lines, like 
-- operator definitions or function pattern matching
multiLineCheck :: [String] -> String -> Bool
multiLineCheck codeLines expected = do
    let func = foldl combine (parseRuby (Unresolved (head codeLines))) (tail codeLines)
        actual = toRuby . fst . unsafePerformIO $ runStateT func defaultState
    if actual == expected
        then True
        else error ("expected: " ++ expected ++ "\ngot: " ++ actual)


main = hspec $ do
  -- describe "composition" $ do
  --     it "check composition parser" $ do
  --       checkParser parseComposition "a . b . c " "a(b(c(x)))"
    
  describe "general parsers" $ do
    bulkCheck $
      [("data types", "data Maybe val", "Maybe = Struct.new(:val)"),
       ("enums", "enum RED | GREEN | BLUE", "RED = :red\nGREEN = :green\nBLUE = :blue"),
       ("method signatures", "double :: Num -> Num", "Contract Num => Num")
      ]
  describe "functions" $ do
    bulkCheck $
      [("simple definition", "add a b := a + b", "def add(a, b)\n  a + b\nend"),
       ("simple definition with leading space", "  add a b := a + b", "def add(a, b)\n  a + b\nend"), -- for indented code, for example
       ("call with an fmap inside the call", "sum(.to_f . .chomp <$> File.readlines(filename))", "sum(File.readlines(filename).map { |a| a.chomp.to_f })"),
       ("call with apply instead of parenthesis", "sum $ .to_f . .chomp <$> File.readlines(filename)", "sum(File.readlines(filename).map { |a| a.chomp.to_f })"),
       ("function call with parens", "hello()", "hello()")
      ]
    
  describe "function composition" $ do
      bulkCheck $
        [("with apply in a function", "parse file := JSON.parse . File.read $ file", "def parse(file)\n  JSON.parse(File.read(file))\nend"),
         ("in a function", "parse := JSON.parse . File.read", "def parse(a)\n  JSON.parse(File.read(a))\nend"),
         ("with apply", "JSON.parse . File.read $ filename", "JSON.parse(File.read(filename))"),
         ("with more than two functions", "a . b . c $ 5", "a(b(c(5)))"),
         ("in a block", "(1..10).map(&(incr . incr))", "(1..10).map { |a| incr(incr(a)) }"),
         ("with fmap", "incr . incr <$> (1..10)", "(1..10).map { |a| incr(incr(a)) }"),
         ("functions that take blocks as an arg", "map list &blk := list.map(&blk.call)", "def map(list, &blk)\n  list.map { |a| blk.call(a) }\nend")
        ]
  describe "currying" $ do
      bulkCheck $
        [("one argument", "incr := add(1, _)", "def incr(a)\n  add(1, a)\nend"),
         ("the only argument", "incrDup := incr(_)", "def incrDup(a)\n  incr(a)\nend"),
         ("the first argument", "incr := add(_, 1)", "def incr(a)\n  add(a, 1)\nend"),
         ("two args", "incr := add(_, 1, _)", "def incr(a, b)\n  add(a, 1, b)\nend"),
         ("with this function already takes an arg", "incr a := add(1, _)", "def incr(a, b)\n  add(1, b)\nend"),
         ("with no parenthesis", "incr := add 1, _", "def incr(a)\n  add(1, a)\nend"),
         ("with no parenthesis, but enclosed in parens", "incr := (add 1, _)", "def incr(a)\n  add(1, a)\nend"),
         ("a single-arg function in a block", "(1..10).map(&incr)", "(1..10).map { |a| incr(a) }"),
         ("a single-arg function in a block (explicit)", "(1..10).map(&incr(_))", "(1..10).map { |a| incr(a) }"),
         ("with fmap", "incr <$> (1..10)", "(1..10).map { |a| incr(a) }"),
         ("one arg in a block", "(1..10).map(&add(1, _))", "(1..10).map { |a| add(1, a) }"),
         ("leave regular ruby syntactical sugar alone", "(1..10).map(&:even?)", "(1..10).map(&:even?)"),
         ("one arg when the other is a symbol", "(1..10).map(&add(:foo, _))", "(1..10).map { |a| add(:foo, a) }")
        ]

  describe "regular ruby" $ do
      bulkCheck $
        [("defs with a block shouldn't get parsed as a block function call", "def method_missing(*args, &block)", "def method_missing(*args, &block)"),
        ("string interpolation should get parsed correctly", "\"hello #{1 + 1}\"", "\"hello #{1 + 1}\""),
        ("string interpolation with a nested string", "\"hello #{name \"adit\"}\"", "\"hello #{name \"adit\"}\""),
        ("string interpolation with nested salmon", "\"hello #{incr <$> (1..10)}\"", "\"hello #{(1..10).map { |a| incr(a) }}\"")
        ]

  describe "infix" $ do
      bulkCheck $
        [("using a function as infix", "1 `add` 2", "add(1, 2)")]
      it "defining a custom operator and using it" $ do
        multiLineCheck ["op <+> add",
                        "a <+> b"] "add(a, b)"

  describe "mixing ruby and salmon" $ do
      bulkCheck $
        [("assigning vars", "a = incr <$> (1..10)", "a = (1..10).map { |a| incr(a) }"),
         ("functions with fmap", "p incr <$> (1..10)", "p (1..10).map { |a| incr(a) }")
        ]

  describe "pattern matching for functions" $ do
    it "simple matching" $ do
        multiLineCheck ["fact 1 := 1",
                        "fact x := x * fact(x - 1)"] "def fact(x)\n  if x == 1\n    1\n  else\n    x * fact(x - 1)\n  end\nend"

    it "pattern matching on one var only" $ do
        multiLineCheck ["fact 1 y := 1",
                        "fact x y := x * fact(x - 1)"] "def fact(x, y)\n  if x == 1\n    1\n  else\n    x * fact(x - 1)\n  end\nend"
