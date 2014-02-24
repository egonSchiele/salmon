{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Parsers where

import Common
import Types
import Utils
import qualified Debug.Trace as D

tryChoice parsers = choice $ map try parsers
tr str = D.trace str (return ())

-- | Parses a constructor (like the `Just a` part of `data Maybe = Nothing | Just a`)
parseClass :: RubyParser
parseClass = do
    name <- string "data" >> whitespace >> parseAtom
    whitespace
    fields <- parseAtom `sepBy` whitespace
    return $ Class name fields

-- | Parses something like `Just "val"`
parseNew :: CodeState -> RubyParser
parseNew state = do
    let classNames = map className (state ^. classes)
    className_ <- choice (map string classNames)
    char ' '
    params_ <- (many1 $ noneOf " =,") `sepBy1` space
    return $ New className_ (map Unresolved params_)

-- | assumes that this section contains only pure ruby.
-- Always succeeds.
parseId :: RubyParser
parseId = do
    line <- option "" (many1 $ noneOf " \t\n")
    return $ String line

parseString :: RubyParser
parseString = parseStringType '\'' <|> parseStringType '"'

parseStringType :: Char -> RubyParser
parseStringType chr = do
    char chr
    x <- many (noneOf [chr])
    char chr
    return . String $ [chr] ++ x ++ [chr]

validChars = "_&?!."

parseAtom :: Stream s m Char => ParsecT s u m String
parseAtom = do
    first <- alphaNum
    rest <- many $ alphaNum <|> (oneOf validChars)
    return $ first:rest

maybeUnwrap parsed = if length parsed == 1
                         then head parsed
                         else List parsed

checkForFmap parsed = case elemIndex (String "<$>") parsed of
                        Nothing -> parsed
                        Just i -> newParsed
                          where front = take (i - 1) parsed
                                back  = takeEnd (length parsed - (i + 2)) parsed
                                prev  = parsed !! (i - 1)
                                next  = parsed !! (i + 1)
                                cur   = parsed !! i
                                newParsed = front ++ [next, String ".map", BlockFunction prev] ++ back

checkForApply parsed = case elemIndex (String "$") parsed of
                         Nothing -> parsed
                         Just i -> case prev of
                                     Parens (Composition n a) -> front ++ [Composition n (Just next)] ++ back
                                     Composition n a -> front ++ [Composition n (Just next)] ++ back
                           where front = take (i - 1) parsed
                                 back  = takeEnd (length parsed - (i + 2)) parsed
                                 prev  = parsed !! (i - 1)
                                 next  = parsed !! (i + 1)
                                 cur   = parsed !! i

parseList :: CodeState -> RubyParser
parseList state = do
    parsed <- (parseExpr state) `sepBy` whitespace
    tr ("parsed from parseList: " ++ show parsed)
    return $ maybeUnwrap . checkForApply . checkForFmap $ parsed

parseBracketed :: CodeState -> RubyParser
parseBracketed state = do
    char '('
    x <- parseList state
    char ')'
    case x of
      BlockFunction _ -> return x
      _ -> return $ Parens x

-- TODO embedded???
parseLine :: CodeState -> RubyParser
parseLine state = parseComment
      <||> parseClass
      <||> parseFunction
      <||> parseOperatorDef
      <||> parseEnum
      <||> parseContract
      <||> do
        parsed <- parseList state
        rest <- many anyChar
        if rest /= ""
          then return $ List [parsed, Unresolved rest]
          else return $ parsed

parseExpr :: CodeState -> RubyParser
parseExpr state = parseString
      <||> parseBracketed state
      <||> parseCurriedFunction
      <||> parseFunctionCall
      <||> parseBlockFunction
      <||> parseComposition
      <||> parseNew state
      <||> parseInfixCall
      <||> parseOperatorUse state
      <||> liftM Atom parseAtom
      <||> parseId

-- If we have a function call with PARENTHESIS, the parseList function
-- won't parse it correctly into an atom and an unresolved...so this
-- function exists. DON'T REMOVE THE char '('...it should only parse
-- function calls that use parenthesis!
parseFunctionCall :: RubyParser
parseFunctionCall = do
    period <- option "" (string ".")
    name <- parseAtom
    char '('
    rest <- many1 anyChar
    return $ List [Atom $ period ++ name, Unresolved $ "(" ++ rest]

parseFunction :: RubyParser
parseFunction = do
    name <- parseAtom
    whitespace
    args <- parseAtom `endBy` whitespace
    string ":=" >> whitespace
    body_ <- many1 anyChar
    return $ Function name args (Unresolved body_)

parseInfixCall :: RubyParser
parseInfixCall = do
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
parseOperatorDef :: RubyParser
parseOperatorDef = do
    string "op "
    op <- manyTill anyChar (try $ char ' ')
    alphaName_ <- many1 anyChar
    return $ Operator op alphaName_

-- gets passed in a bunch of custom defined operators, like <||> or
-- whatever. Checks to see if its used anywhere, so it can get substituted
-- with the correct alphaName
-- TODO almost exactly like `infixCallParser`, refactor?
parseOperatorUse :: CodeState -> RubyParser
parseOperatorUse state = do
    let ops = state ^. operators
        opNames = map operator ops
    left <- manyTill anyChar (try $ char ' ')
    opName <- choice (map string opNames)
    char ' '
    right <- many1 anyChar
    return $ InfixCall (Unresolved left) (fromJust $ findAlphaName opName ops) (Unresolved right)

parseEnum :: RubyParser
parseEnum = do
    string "enum "
    options <- parseAtom `sepBy1` (string " | ")
    return $ Enum options

parseContract :: RubyParser
parseContract = do
    parseAtom >> whitespace >> string "::" >> whitespace
    params <- (many1 $ noneOf " ") `sepBy1` (whitespace >> string "->" >> whitespace)
    return $ Contract (init params) (last params)

parseComment :: RubyParser
parseComment = do
    leadingSpace <- option "" (many1 space)
    char '#'
    rest <- many1 anyChar
    return $ Atom (leadingSpace ++ "#" ++ rest)

-- A block function looks like this: `&incr` i.e. a part of
-- `(1..10).map(&incr)`. This can optionally be enclosed in parens or not:
-- `&incr` and `&(incr . incr)` are both valid. However, if, and only if,
-- there is an opening paren, there must be a closing paren.
parseBlockFunction :: RubyParser
parseBlockFunction = do
    char '&'
    openingParen <- optionMaybe $ char '('
    curriedFunc <- parseComposition <||> parseCurriedFunction <||> parseCurriedFunctionSingleArg
    case openingParen of
      Nothing -> return ()
      Just _ -> char ')' >> return ()
    return $ BlockFunction curriedFunc

parseCurriedFunction :: RubyParser
parseCurriedFunction = do
    name <- parseAtom
    (optional $ char '(') <|> whitespace
    args <- (parseAtom <|> string "_") `sepBy1` (spaces >> char ',' >> spaces)
    optional $ char ')'
    if ("_" `notElem` args)
      then fail "Not a curried function, didn't find an underscore (_)"
      else return $ CurriedFunction name (map (\a -> if a == "_" then (Atom a) else (Unresolved a)) args)

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
parseCurriedFunctionSingleArg :: RubyParser
parseCurriedFunctionSingleArg = do
    name <- parseAtom
    return $ CurriedFunction name [Atom "_"]

-- add a b := a + b

-- def add(a, b)
--   a + Composition {functionNames = ["b"], argument = Nothing}
-- end

parseComposition :: RubyParser
parseComposition = do
    names <- parseAtom `sepBy1` (spaces >> char '.' >> spaces)
    if length names < 2
      then fail "Not a composition since there's only one function!!"
      else return $ Composition names Nothing
