{-# LANGUAGE FlexibleContexts #-}

module Parsers where

import Common
import Types
import Utils

betweenSpaces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenSpaces p = between spaces spaces p

constructorName :: Stream s m Char => ParsecT s u m Constructor
constructorName = do
    name_ <- many1 alphaNum
    spaces
    fields_ <- (many1 alphaNum) `sepBy` (many1 space)
    return $ Constructor name_ fields_

dataParser :: Stream s m Char => ParsecT s u m [Constructor]
dataParser = do
    string "data"
    spaces
    superclass <- many1 alphaNum
    betweenSpaces (char '=')
    constructors <- constructorName `sepBy` (betweenSpaces (char '|'))
    return constructors
