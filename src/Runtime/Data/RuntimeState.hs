{-# LANGUAGE TemplateHaskell #-}
module Runtime.Data.RuntimeState where

import           Control.Lens

import           Parser.MoorbenParser
import           Runtime.Data.BallState
import           Runtime.Data.RuntimeOptions

data RuntimeState = RuntimeState
  { _sourceCode :: SourceCode
  , _options    :: RuntimeOptions
  , _balls      :: [BallState]
  , _tapes      :: Tapes
  } deriving (Show, Eq)

data Tapes = Tapes Int [TapeStack] deriving (Show, Eq)
data TapeStack = TapeStack [StackItem] deriving (Show, Eq)
data StackItem = StackChar Char
                | StackBool Bool
                | StackInt Int
                deriving (Show, Eq)

makeLenses ''RuntimeState
