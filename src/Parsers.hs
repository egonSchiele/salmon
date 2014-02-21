{-# LANGUAGE FlexibleContexts #-}

module Parsers where

import Common
import Types
import Utils

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
    params_ <- (many1 anyChar) `sepBy` space
    let className_ = firstLetter:name_
    return $ New className_ (map Unresolved params_)

-- | assumes that this section contains only pure ruby.
-- Always succeeds.
idParser :: RubyParser
idParser = do
    line <- option "" (many1 anyChar)
    return $ Identifier line

embeddedParser :: RubyParser
embeddedParser = do
    let parsers = choice [newParser]
    front <- manyTill anyChar (try . lookAhead $ parsers )
    ruby <- parsers
    rest <- embeddedParser <||> idParser
    return $ Embedded [Identifier front, ruby, rest]

functionParser :: RubyParser
functionParser = do
    name_ <- many1 alphaNum
    char ' '
    args_ <- (many1 anyChar) `sepBy` space
    string " = "
    body_ <- many1 anyChar
    return $ Function name_ args_ (Unresolved body_)
