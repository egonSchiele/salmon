{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Parsers where

import Common
import Types
import Utils
import qualified Debug.Trace as D

tryChoice parsers = choice $ map try parsers

betweenSpaces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenSpaces p = between spaces spaces p

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
newParser :: RubyParser
newParser = do
    firstLetter <- upper
    name_ <- many1 alphaNum
    char ' '
    params_ <- (many1 $ noneOf " ") `sepBy` space
    let className_ = firstLetter:name_
    return $ New className_ (map Unresolved params_)

-- | assumes that this section contains only pure ruby.
-- Always succeeds.
idParser :: RubyParser
idParser = do
    line <- option "" (many1 anyChar)
    return $ Identifier line

embeddedParser :: [Ruby] -> RubyParser
embeddedParser ops = do
    let parsers = tryChoice [newParser, infixCallParser, operatorUseParser ops]
    front <- manyTill anyChar (try . lookAhead $ parsers)
    ruby <- parsers
    rest <- (embeddedParser ops) <||> idParser
    return $ Embedded [Identifier front, ruby, rest]

functionParser :: RubyParser
functionParser = do
    name_ <- manyTill alphaNum (char ' ')
    args_ <- ((many1 alphaNum) `endBy1` space)
    string "= "
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
