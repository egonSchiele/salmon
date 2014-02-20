{-# LANGUAGE TemplateHaskell #-}

module Types where

import Utils
import Common

data Constructor = Constructor {
                     name :: String,
                     fields :: [String]
} deriving (Show)

class Ruby a where
    toRuby :: a -> String

instance Ruby Constructor where
    toRuby (Constructor n f) = printf "%s = Struct.new(%s)" n (join "," $ map (\name -> ":" ++ name) f)

data CodeState = CodeState {
                   _classes :: [Constructor]
}

makeLenses  ''CodeState

defaultState = CodeState []


