{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Parsers where

import Common
import Types
import Utils
import qualified Debug.Trace as D

tryChoice parsers = choice $ map try parsers

betweenSpaces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenSpaces p = between spaces spaces p

identifier :: Stream s m Char => ParsecT s u m Char
identifier = alphaNum <|> (oneOf "-_.&?!")

-- | Parses a constructor (like the `Just a` part of `data Maybe = Nothing | Just a`)
classParser :: RubyParser
classParser = do
    string "data"
    spaces
    name_ <- many1 alphaNum
    spaces
    fields_ <- (many1 alphaNum) `sepBy` (many1 space)
    return $ Class name_ fields_

-- | Parses something like `Just "val"`
newParser :: [Ruby] -> RubyParser
newParser cls = do
    let classNames = map name cls
    className_ <- choice (map string classNames)
    char ' '
    params_ <- (many1 $ noneOf " =,") `sepBy1` space
    return $ New className_ (map Unresolved params_)

-- | assumes that this section contains only pure ruby.
-- Always succeeds.
idParser :: RubyParser
idParser = do
    line <- option "" (many1 anyChar)
    return $ Identifier line

embeddedParser :: CodeState -> RubyParser
embeddedParser state = do
    let ops = state ^. operators
        cls = state ^. classes
        parsers = tryChoice [stringParser, fmapParser, blockCurriedFunctionParser, curriedFunctionParser, compositionParser, newParser cls, infixCallParser, operatorUseParser ops]
    front <- manyTill anyChar (try . lookAhead $ parsers)
    ruby <- parsers
    rest <- (embeddedParser state) <||> idParser
    let rubies = filter (not . blankIdentifier) [Identifier front, ruby, rest]
    if length rubies == 1
      then return $ head rubies
      else return $ Embedded rubies

stringParser :: RubyParser
stringParser = singleQuoteStringParser <||> doubleQuoteStringParser

singleQuoteStringParser :: RubyParser
singleQuoteStringParser = do
    char '\''
    string <- manyTill anyChar (char '\'')
    return $ Identifier ("'" ++ string ++ "'")

doubleQuoteStringParser :: RubyParser
doubleQuoteStringParser = do
    char '"'
    string <- manyTill anyChar (char '"')
    return $ Identifier ("\"" ++ string ++ "\"")

functionParser :: RubyParser
functionParser = do
    name_ <- manyTill identifier (char ' ')
    args_ <- ((many1 identifier) `endBy` space)
    string ":= "
    body_ <- many1 anyChar
    return $ Function name_ args_ (Unresolved body_)

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
    options <- ((many1 identifier) `sepBy1` (string " | "))
    return $ Enum options

contractParser :: RubyParser
contractParser = do
    manyTill identifier (try $ string " :: ")
    params <- ((many1 $ noneOf " ") `sepBy1` (string " -> "))
    return $ Contract (init params) (last params)

commentParser :: RubyParser
commentParser = do
    leadingSpace <- option "" (many1 space)
    char '#'
    rest <- many1 anyChar
    return $ Identifier (leadingSpace ++ "#" ++ rest)

blockCurriedFunctionParser :: RubyParser
blockCurriedFunctionParser = do
    oneOf "( " >> spaces >> string "&"
    curriedFunc <- curriedFunctionParser <||> curriedFunctionSingleArgParser
    spaces >> (optional $ string ")")
    return $ BlockCurriedFunction curriedFunc

curriedFunctionParser :: RubyParser
curriedFunctionParser = do
    name_ <- manyTill identifier (oneOf "(")
    spaces
    args_ <- many1 (noneOf "(=,)") `sepBy1` (string ", ")
    char ')'
    if ("_" `notElem` args_)
      then fail "Not a curried function, didn't find an underscore (_)"
      else return $ CurriedFunction name_ (map Unresolved args_)

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
    name_ <- manyTill identifier (oneOf " )") <||> manyTill identifier eof
    return $ CurriedFunction name_ [Identifier "_"]

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
    names <- many1 identifier `sepBy1` (string " . ")
    if length names < 2
      then fail "Not a composition since there's only one function!!"
      else return $ Composition names Nothing
