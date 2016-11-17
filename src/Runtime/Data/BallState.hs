{-# LANGUAGE TemplateHaskell #-}

module Runtime.Data.BallState where

import           Control.Lens

import           Runtime.Data.Position
import           Runtime.Data.Velocity

data BallState = BallState
  { _position  :: Position
  , _velocity  :: Velocity
  , _tapeIndex :: Int
  } deriving (Show, Eq)

makeLenses ''BallState
