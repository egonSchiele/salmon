{-# LANGUAGE FlexibleContexts #-}

module Parsers where

import Common
import Types
import Utils

betweenSpaces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenSpaces p = between spaces spaces p

-- | Parses a constructor (like the `Just a` part of `data Maybe = Nothing | Just a`)
constructorName :: Stream s m Char => ParsecT s u m RubyData
constructorName = do
    name_ <- many1 alphaNum
    spaces
    fields_ <- (many1 alphaNum) `sepBy` (many1 space)
    return $ Constructor name_ fields_

-- | Parses something like `data Maybe = Nothing | Just val` and makes
-- classes out of those based on Structs
dataParser :: Stream s m Char => ParsecT s u m [RubyData]
dataParser = do
    string "data"
    spaces
    superclass <- many1 alphaNum
    betweenSpaces (char '=')
    constructors <- constructorName `sepBy` (betweenSpaces (char '|'))
    return constructors

-- | Parses something like `Just "val"` into `Just.new("val")`
newParser :: Stream s m Char => ParsecT s u m [RubyData]
newParser = do
    front <- manyTill anyChar (lookAhead upper)
    first <- upper
    name_ <- many1 alphaNum
    spaces
    params <- (many1 anyChar) `sepBy` (many1 space)
    back <- option "" $ many1 anyChar
    let className = first:name_
        str = printf "%s%s.new(%s)%s" front className (join "," params) back
    return $ [RubyLine str]
