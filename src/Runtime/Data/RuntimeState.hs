{-# LANGUAGE TemplateHaskell #-}
module Runtime.Data.RuntimeState where

import           Control.Lens

import           Parser.MoorbenParser
import           Runtime.Data.OrbState
import           Runtime.Data.RuntimeOptions
import           Runtime.Data.World

data RuntimeState = RuntimeState
  { _sourceCode :: SourceCode
  , _options    :: RuntimeOptions
  , _orbs       :: [OrbState]
  , _tapes      :: Tapes
  , _world      :: World
  } deriving (Show, Eq)

data Tapes = Tapes Int [TapeStack] deriving (Show, Eq)
data TapeStack = TapeStack [StackItem] deriving (Show, Eq)
data StackItem = StackChar Char
                | StackBool Bool
                | StackInt Int
                deriving (Show, Eq)

makeLenses ''RuntimeState
