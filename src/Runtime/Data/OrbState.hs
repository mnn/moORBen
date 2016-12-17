{-# LANGUAGE TemplateHaskell #-}

module Runtime.Data.OrbState where

import           Control.Lens

import           Runtime.Data.Position
import           Runtime.Data.Velocity

data OrbState = OrbState
  { _position  :: Position
  , _velocity  :: Velocity
  , _tapeIndex :: Int
  } deriving (Show, Eq)

makeLenses ''OrbState
