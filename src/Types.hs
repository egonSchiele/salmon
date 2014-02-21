{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}

module Types where

import Utils
import Common

data Ruby = Class {
              name :: String,
              fields :: [String]
            }
            | Fmap {
              item :: Ruby,
              block :: Ruby
            }
            | New {
              className :: String,
              params :: [Ruby]
            }
            | Unresolved String -- some code that hasn't been resolved into a final `Ruby` object yet
            | Identifier String -- like `(1..10)` from `(1..10).map()`. Just something that should be copied directly.
            | Embedded [Ruby]   -- Useful things like `p Just "hello"`, where `p` is an `Identifier` but `Just "hello"` is a `New`.
            | Function {
                functionName :: String,
                args :: [String],
                body :: Ruby
              }
            | InfixCall {
                leftArg :: Ruby,
                infixFunctionName :: String,
                rightArg :: Ruby
            }
            | Operator {
                operator :: String,
                alphaName :: String
            }
            | Enum [String]
            deriving (Show)

toRuby :: Ruby -> String
toRuby (Class n f) = printf "%s = Struct.new(%s)" n params_
  where params_ = if null f
                    then "nil"
                    else (join ", " $ map (\name -> ":" ++ name) f)
toRuby (New c p) = printf "%s.new%s" c params_
  where params_ = if null p
                    then ""
                    else printf "(%s)" (join ", " $ map toRuby p)

toRuby (Identifier str) = str
toRuby (Embedded xs) = concat $ map toRuby xs
toRuby (Function name_ args_ body_) = printf "def %s%s\n  %s\nend" name_ argsStr (toRuby body_)
  where argsStr = if null args_
                    then ""
                    else "(" ++ (join ", " args_) ++ ")"
toRuby (InfixCall left name_ right) = printf "%s(%s, %s)" name_ (toRuby left) (toRuby right)
toRuby (Operator _ _) = ""
toRuby (Enum choices) = join "\n" $ map makeEnum choices
  where makeEnum c  = printf "%s = %s" c (symbolize c)
        symbolize c = ":" ++ (toLower <$> c)
toRuby x = show x

data CodeState = CodeState {
                   _operators :: [Ruby],
                   _code :: [Ruby]
}

makeLenses  ''CodeState

defaultState = CodeState [] []

type RubyParser = Stream s m Char => ParsecT s u m Ruby

findAlphaName op [] = Nothing
findAlphaName op (x:xs) = if (operator x == op)
                            then (Just $ alphaName x)
                            else findAlphaName op xs

-- | Easy way to remove Ruby objects that were for meta information only,
-- and shouldn't be in the code. These objects (like `Operator` which
-- defines new operators) add unnecessary blank lines to the code.
concatRuby :: [Ruby] -> [Ruby]
concatRuby [] = []
concatRuby ((Operator _ _):xs) = concatRuby xs
concatRuby (x:xs) = x:(concatRuby xs)
