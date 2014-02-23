{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Parsers where

import Common
import Types
import Utils
import qualified Debug.Trace as D

tryChoice parsers = choice $ map try parsers

-- | Parses a constructor (like the `Just a` part of `data Maybe = Nothing | Just a`)
classParser :: RubyParser
classParser = do
    name <- string "data" >> whitespace >> parseAtom
    whitespace
    fields <- parseAtom `sepBy` whitespace
    return $ Class name fields

-- | Parses something like `Just "val"`
newParser :: [Ruby] -> RubyParser
newParser cls = do
    let classNames = map className cls
    className_ <- choice (map string classNames)
    char ' '
    params_ <- (many1 $ noneOf " =,") `sepBy1` space
    return $ New className_ (map Unresolved params_)

-- | assumes that this section contains only pure ruby.
-- Always succeeds.
idParser :: RubyParser
idParser = do
    line <- option "" (many1 anyChar)
    return $ Atom line

embeddedParser :: CodeState -> RubyParser
embeddedParser state = do
    let ops = state ^. operators
        cls = state ^. classes
        parsers = tryChoice [parseString, fmapParser, blockFunctionParser, compositionParser, curriedFunctionParser, newParser cls, infixCallParser, operatorUseParser ops]
    front <- manyTill anyChar (try . lookAhead $ parsers)
    D.trace ("front: " ++ (show front)) (return ())
    ruby <- parsers
    D.trace ("ruby: " ++ (show ruby)) (return ())
    rest <- (embeddedParser state) <||> idParser
    D.trace ("rest: " ++ (show rest)) (return ())
    let rubies = filter (not . blankAtom) [Atom front, ruby, rest]
    if length rubies == 1
      then return $ head rubies
      else return $ List rubies

parseString :: RubyParser
parseString = parseStringType '\'' <|> parseStringType '"'

parseStringType :: Char -> RubyParser
parseStringType chr = do
    char chr
    x <- many (noneOf [chr])
    char chr
    return $ String x

parseAtom :: Stream s m Char => ParsecT s u m String
parseAtom = many1 $ alphaNum <|> (oneOf "-_&?!.")

parseList :: RubyParser
parseList = liftM List $ sepBy parseLine whitespace

parseBracketed :: RubyParser
parseBracketed = do
    char '('
    x <- parseList
    char ')'
    return x

parseLine :: RubyParser
parseLine = liftM Atom parseAtom
       <|> parseString
       <|> parseBracketed

functionParser :: RubyParser
functionParser = do
    name <- parseAtom
    args <- parseAtom `endBy` whitespace
    string ":=" >> whitespace
    body_ <- many1 anyChar
    return $ Function name args (Unresolved body_)

infixCallParser :: RubyParser
infixCallParser = do
    left <- manyTill anyChar (try $ char ' ')
    name_ <- between (char '`') (char '`') (many1 alphaNum)
    char ' '
    right <- many1 anyChar
    return $ InfixCall (Unresolved left) name_ (Unresolved right)

-- | You can define custom operators like this:
--
-- > op <+> add
--
-- Now you can use `2 <+> 3` instead of `add(2, 3)`.
--
-- This is what parses the `op <+> add` statement.
operatorParser :: RubyParser
operatorParser = do
    string "op "
    op <- manyTill anyChar (try $ char ' ')
    alphaName_ <- many1 anyChar
    return $ Operator op alphaName_

-- gets passed in a bunch of custom defined operators, like <||> or
-- whatever. Checks to see if its used anywhere, so it can get substituted
-- with the correct alphaName
-- TODO almost exactly like `infixCallParser`, refactor?
operatorUseParser :: [Ruby] -> RubyParser
operatorUseParser ops = do
    let opNames = map operator ops
    left <- manyTill anyChar (try $ char ' ')
    opName <- choice (map string opNames)
    char ' '
    right <- many1 anyChar
    return $ InfixCall (Unresolved left) (fromJust $ findAlphaName opName ops) (Unresolved right)

enumParser :: RubyParser
enumParser = do
    string "enum "
    options <- parseAtom `sepBy1` (string " | ")
    return $ Enum options

contractParser :: RubyParser
contractParser = do
    parseAtom >> whitespace >> string "::" >> whitespace
    params <- (many1 $ noneOf " ") `sepBy1` (whitespace >> string "->" >> whitespace)
    return $ Contract (init params) (last params)

commentParser :: RubyParser
commentParser = do
    leadingSpace <- option "" (many1 space)
    char '#'
    rest <- many1 anyChar
    return $ Atom (leadingSpace ++ "#" ++ rest)

blockFunctionParser :: RubyParser
blockFunctionParser = do
    oneOf "( " >> spaces >> string "&"
    curriedFunc <- compositionParser <||> curriedFunctionParser <||> curriedFunctionSingleArgParser
    spaces >> (optional $ char ')')
    return $ BlockFunction curriedFunc

curriedFunctionParser :: RubyParser
curriedFunctionParser = do
    name <- parseAtom
    (optional $ char '(') <|> whitespace
    args <- parseAtom `sepBy1` (spaces >> char ',' >> spaces)
    optional $ char ')'
    if ("_" `notElem` args)
      then fail "Not a curried function, didn't find an underscore (_)"
      else return $ CurriedFunction name (map Unresolved args)

-- Sometimes, you want to pass in a function name somewhere. This function
-- takes one argument, so you could pass it in like this:
--
-- > (1..10).map(&increment(_))
--
-- But it would be nicer to pass it in like this:
--
-- > (1..10).map(&increment)
--
-- The only two places where this is legal: blocks and function
-- composition. For example, you can't do this:
--
-- > incrDup := increment
--
-- Because I don't know that `increment` is meant to be a function in this
-- context. But in blocks and function composition, we *know* its
-- a function. So leaving off the _ is ok. This parser just parses an
-- identifier and makes a curried function out of it, where the curried
-- function just takes one argument. This is the kind of shit you have to
-- do in ruby.
curriedFunctionSingleArgParser :: RubyParser
curriedFunctionSingleArgParser = do
    name <- parseAtom
    return $ CurriedFunction name [Atom "_"]

fmapParser :: RubyParser
fmapParser = do
    function <- manyTill (noneOf "<") (string " <$> ")
    item <- many1 anyChar
    let str = printf "%s.map(&%s)" item function
    return $ Unresolved str

-- add a b := a + b

-- def add(a, b)
--   a + Composition {functionNames = ["b"], argument = Nothing}
-- end

compositionParser :: RubyParser
compositionParser = do
    names <- parseAtom `sepBy1` (spaces >> char '.' >> spaces)
    if length names < 2
      then fail "Not a composition since there's only one function!!"
      else return $ Composition names Nothing
