{-# LANGUAGE TemplateHaskell #-}

module Runtime.Data.Velocity where

import           Control.Lens

data Velocity = Velocity
 { _x :: Int
 , _y :: Int
 } deriving (Show)

makeLenses ''Velocity
