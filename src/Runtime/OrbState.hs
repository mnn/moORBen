{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Runtime.OrbState where

import           Control.Lens

import           Runtime.Position
import           Runtime.Velocity

data OrbState = OrbState
  { _orbStatePosition  :: Position
  , _orbStateVelocity  :: Velocity
  , _orbStateTapeIndex :: Int
  } deriving (Show, Eq)

makeFields ''OrbState
