{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}

module Types where

import Utils
import Common
import qualified Debug.Trace as D

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
            | Contract {
                input :: [String],
                output :: String
            }
            -- Currying = only acceptable when:
            -- 1. assigning to a function name, OR
            -- 2. being passed in to a block.
            | CurriedFunction {
                curriedFunctionName :: String,
                curriedArgs :: [Ruby]
            }
            | BlockCurriedFunction Ruby -- special case...if a curried function is passed in as a block, render it differently.
            | Composition {
                functionNames :: [String],
                argument :: Maybe Ruby
            }
            deriving (Show, Eq)


missingArgs :: Ruby -> [Ruby]
missingArgs (CurriedFunction _ cfArgs) = filter (== (Identifier "_")) cfArgs

-- all the letters of the alphabet, as strings instead of chars.
alphabets :: [String]
alphabets = (map (\c -> [c]) ['a'..'z'])

-- placeholderArgsFor = for every underscore (_) in this curried function,
-- choose a symbol for the arg. Make sure its not a symbol we have used already.
-- Return all the chosen symbols.
placeholderArgsFor :: Ruby -> [String] -> [String]
placeholderArgsFor c@(CurriedFunction n cfArgs) args_ = take (length $ missingArgs c) (alphabets \\ args_)
placeholderArgsFor c@(Composition _ Nothing) args_ = take 1 (alphabets \\ args_)
placeholderArgsFor _ _ = []

makeCompositionString [] arg = toRuby arg
makeCompositionString (f:funcs) arg = printf "%s(%s)" f (makeCompositionString funcs arg)

-- blend the curried function's args and our made up args. So if
-- the curried function's arg is an underscore (_), then use
-- a made up arg. Else use the curried function's arg. This way,
-- I can do currying of any position.
blend xs ys = blend_ xs ys []
blend_ [] _ acc = acc
blend_ ((Identifier "_"):xs) (y:ys) acc = blend_ xs ys (acc ++ [Identifier y])
blend_ (x:xs) ys acc = blend_ xs ys (acc ++ [x])

class ConvertToRuby a where
    toRuby :: a -> String

instance ConvertToRuby Ruby where
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

  -- if the body is a curried function, this
  -- function needs to take more params to pass
  -- in to the curried function.
  toRuby (Function name_ args_ body_) = printf "def %s%s\n  %s\nend" name_ argsStr bodyStr
    -- placeholderArgsFor will make args for currying, OR args for function
    -- composition. None of the functions in the function composition can
    -- be curried, so don't need to worry about that!
    where extraArgs = placeholderArgsFor body_ args_
          allArgs = args_ ++ extraArgs
          argsStr = if null allArgs
                      then ""
                      else "(" ++ (join ", " allArgs) ++ ")"
          bodyStr = case body_ of
                      (CurriedFunction cfName cfArgs) -> printf "%s(%s)" cfName (join ", " (map toRuby (blend cfArgs extraArgs)))
                      (Composition funcs (Just arg)) -> makeCompositionString funcs arg
                      (Composition funcs Nothing)    -> makeCompositionString funcs (Identifier . head $ extraArgs)
                      _ -> toRuby body_

  toRuby (InfixCall left name_ right) = printf "%s(%s, %s)" name_ (toRuby left) (toRuby right)
  toRuby (Operator _ _) = ""
  toRuby (Enum choices) = join "\n" $ map makeEnum choices
    where makeEnum c  = printf "%s = %s" c (symbolize c)
          symbolize c = ":" ++ (toLower <$> c)
  toRuby (Contract inp out) = printf "Contract %s => %s" (join ", " inp) out
  toRuby c@(CurriedFunction cfName cfArgs) = error $ "Curried functions can only be used as a block, or if you assign it a name by making it into a new function. You can't use curried functions in function composition etc. We don't make lambdas out of curried functions simply because it doesn't look like idiomatic Ruby. You're seeing this because you might have tried to make this curried function into a lambda: " ++ show c

  toRuby (BlockCurriedFunction c@(CurriedFunction cfName cfArgs)) = printf " { |%s| %s(%s) }" (join "," curryArgs) cfName (join ", " (map toRuby (blend cfArgs curryArgs)))
    where curryArgs = placeholderArgsFor c []
    
  toRuby x = show x

data Extra = Contracts deriving (Show, Eq)

instance ConvertToRuby Extra where
    toRuby Contracts = "require 'rubygems'\nrequire 'contracts'\ninclude Contracts\n"

data CodeState = CodeState {
                   _operators :: [Ruby],
                   _classes :: [Ruby],
                   _code :: [Ruby],
                   _headExtras :: [Extra]
}

makeLenses  ''CodeState

defaultState = CodeState [] [] [] []

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

blankIdentifier (Identifier "") = True
blankIdentifier _ = False
