{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Parsers where

import Common
import Types
import Utils
import qualified Debug.Trace as D

tryChoice parsers = choice $ map try parsers
tr str = return ()
-- tr str = D.trace str (return ())

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

-- parses a function arg, like `a`, `str`, `&blk` or `*args`
parseFunctionArg :: Stream s m Char => ParsecT s u m String
parseFunctionArg = do
    first <- alpha <|> oneOf "*&"
    rest <- many alphaNum
    return $ first:rest

parseAtom :: Stream s m Char => ParsecT s u m String
parseAtom = do
    first <- alphaNum
    rest <- many $ alphaNum <|> (oneOf validChars)
    return $ first:rest

-- return something like ".hello"...used in function composition.
parseCompositionMethod :: Stream s m Char => ParsecT s u m String
parseCompositionMethod = do
    first <- char '.'
    rest <- many1 $ alphaNum <|> (oneOf validChars)
    return $ first:rest

maybeUnwrap parsed = if length parsed == 1
                         then head parsed
                         else List parsed

checkForFmap parsed = case elemIndex (String "<$>") parsed of
                        Nothing -> parsed
                        Just i -> newParsed
                          -- for all of these, we actually skip the two
                          -- values around <$> because those are spaces
                          -- that we added ourselves.
                          where front = take (i - 2) parsed
                                back  = takeEnd (length parsed - (i + 3)) parsed
                                next  = parsed !! (i + 2)
                                cur   = parsed !! i
                                -- If its something like `incr <$>
                                -- (1..10)`, `incr` will get parsed as an
                                -- atom but in this case we can assume we
                                -- meant a curried function because the two
                                -- possibilities are curried func or
                                -- composed func.
                                prev  = case parsed !! (i - 2) of
                                          (Atom x) -> CurriedFunction x [Atom "_"]
                                          x -> x
                                newParsed = front ++ [next, String ".map", BlockFunction prev] ++ back

checkForApply parsed = case elemIndex (String "$") parsed of
                         Nothing -> parsed
                         Just i -> case prev of
                                     Parens (Composition n a) -> front ++ [Composition n (Just $ List rest)]
                                     Composition n a -> front ++ [Composition n (Just $ List rest)]
                                     Atom funcName -> front ++ [Composition [funcName] (Just $ List rest)]
                                     String funcName -> front ++ [Composition [funcName] (Just $ List rest)]
                                     _ -> parsed
                          -- for all of these, we actually skip the two
                          -- values around <$> because those are spaces
                          -- that we added ourselves.
                           where front = take (i - 2) parsed
                                 rest  = takeEnd (length parsed - (i + 2)) parsed
                                 prev  = parsed !! (i - 2)
                                 cur   = parsed !! i

parseList :: CodeState -> RubyParser
parseList state = do
    parsed <- (parseExpr state) `sepBy` whitespace
    tr ("parsed from parseList: " ++ show parsed)
    return $ maybeUnwrap . checkForApply . checkForFmap . intersperse (String " ") $ parsed

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

parseCaseFunction :: RubyParser
parseCaseFunction = do
    name <- parseAtom
    whitespace
    args <- (many1 $ noneOf " ") `endBy` whitespace
    string ":=" >> whitespace
    body_ <- many1 anyChar
    return $ CaseFunction name args (Unresolved body_)

parseFunction :: RubyParser
parseFunction = do
    name <- parseAtom
    whitespace
    args <- parseFunctionArg `endBy` whitespace
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
    -- Don't want to accidentally parse the block in a function def, like
    -- `def map(list, &blk)` or in our case, `map list &blk :=`
    notFollowedBy $ whitespace >> string ":="
    return $ BlockFunction curriedFunc

parseCurriedFunction :: RubyParser
parseCurriedFunction = do
    name <- parseAtom
    opening <- oneOf "( "
    args <- (many1 $ noneOf " ,)") `sepBy1` (spaces >> char ',' >> spaces)
    case opening of
      ' ' -> return ()
      '(' -> char ')' >> return ()
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
    names <- (parseAtom <|> parseCompositionMethod) `sepBy1` (try $ spaces >> char '.' >> spaces)
    if length names < 2
      then fail "Not a composition since there's only one function!!"
      else return $ Composition names Nothing

maybeModifyState o@(Operator _ _) = modify $ over operators (o:)
maybeModifyState c@(Class _ _) = modify $ over classes (c:)
maybeModifyState c@(Contract _ _) = modify $ over headExtras (union [Contracts])
maybeModifyState f@(CaseFunction name args body) = modify $ over functions (f:)
maybeModifyState x = return ()

-- | This is what computes the AST. The main method is the one that parses
-- `Unresolved` objects. There are others which will take an existing Ruby
-- object, and check if any parts of it are unresolved. If so, it feeds
-- them back into `parseRuby`.
parseRuby :: Ruby -> StateT CodeState IO Ruby
parseRuby (Unresolved line) = do
  state <- get
  case parse (parseLine state) "" line of
      Left err -> error (show err)
      Right result -> do
                newResult <- parseRuby result
                maybeModifyState newResult
                tr $ show result
                newResult

-- debugging
parseRuby i@(Atom line) = do
    tr $ show i
    return i

parseRuby (CurriedFunction n cfArgs) = do
    newCfArgs <- mapM parseRuby cfArgs
    return $ CurriedFunction n newCfArgs

parseRuby (Composition funcs (Just arg)) = do
    newArg <- parseRuby arg
    return $ Composition funcs (Just newArg)

parseRuby (BlockFunction f) = do
    newF <- parseRuby f
    return $ BlockFunction newF

parseRuby (New c params_) = do
    newParams <- mapM parseRuby params_
    return $ New c newParams

parseRuby (Parens x) = do
    newX <- parseRuby x
    return $ Parens newX

parseRuby (List xs) = do
    newXs <- mapM parseRuby xs
    return $ List newXs

combineFuncBodies newFunction@(Function n a body) caseFunc = 

parseRuby (Function n a b@(Unresolved _)) = do
    newBody <- parseRuby b

    -- check if we have a case for this function
    state <- get
    let functions_ = state ^. functions
        cases = filter ((== n) . caseFunctionName) functions
        newFunction = Function n a newBody
        forM_ cases $ \caseFunc -> do
    return $ 

parseRuby (CaseFunction n a b@(Unresolved _)) = do
    newBody <- parseRuby b
    return $ CaseFunction n a newBody

parseRuby (InfixCall left name_ right) = do
    newLeft <- parseRuby left
    newRight <- parseRuby right
    return $ InfixCall newLeft name_ newRight

parseRuby x = return x

isAtom str = case parse parseAtom "" str of
               Left _ -> False
               Right _ -> True
