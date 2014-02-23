{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Common (
 module Text.Parsec
,module Data.List
,module System.Environment
,module Control.Applicative
,module Text.Printf
,module Control.Monad
,module Control.Monad.State
,module Control.Lens
,module Data.Maybe
,module System.Directory
,module Data.Char
,module Data.Monoid
,whitespace
) where

import Text.Parsec hiding (State, uncons)
import Data.List
import System.Environment
import Control.Applicative hiding (optional, (<|>), many)
import Text.Printf
import Control.Monad hiding (join)
import Control.Monad.State hiding (join)
import Control.Lens
import Data.Maybe
import System.Directory
import Data.Char
import Data.Monoid

whitespace :: Stream s m Char => ParsecT s u m ()
whitespace = skipMany1 space
