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
  } deriving (Show)

makeLenses ''RuntimeState
