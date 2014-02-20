{-# LANGUAGE TemplateHaskell #-}

module Types where

import Utils
import Common

data RubyData = Constructor {
                  name :: String,
                  fields :: [String]
                }
                | RubyLine String
                | UnchangedLine String

class Ruby a where
    toRuby :: a -> String

instance Ruby RubyData where
    toRuby (Constructor n f) = printf "%s = Struct.new(%s)" n params
              where params = if null f
                               then "nil"
                               else (join "," $ map (\name -> ":" ++ name) f)
    toRuby (RubyLine str) = str
    toRuby (UnchangedLine str) = str

data CodeState = CodeState {
                   _classes :: [RubyData]
}

makeLenses  ''CodeState

defaultState = CodeState []
