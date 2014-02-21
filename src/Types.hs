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
            deriving (Show)

toRuby :: Ruby -> String
toRuby (Class n f) = printf "%s = Struct.new(%s)" n params_
  where params_ = if null f
                    then "nil"
                    else (join "," $ map (\name -> ":" ++ name) f)
toRuby (New c p) = printf "%s.new%s" c params_
  where params_ = if null p
                    then ""
                    else printf "(%s)" (join "," $ map toRuby p)

toRuby (Identifier str) = str
toRuby (Embedded xs) = concat $ map toRuby xs
toRuby (Function name_ args_ body_) = printf "def %s(%s)\n  %s\nend" name_ (join ", " args_) (toRuby body_)
toRuby x = show x

data CodeState = CodeState {
                   _code :: [Ruby]
}

makeLenses  ''CodeState

defaultState = CodeState []

type RubyParser = Stream s m Char => ParsecT s u m Ruby
