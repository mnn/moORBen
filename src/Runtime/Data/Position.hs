{-# LANGUAGE TemplateHaskell #-}

module Runtime.Data.Position where

import           Control.Lens

data Position = Position
  { _x :: Int
  , _y :: Int
  } deriving (Show, Eq)

makeLenses ''Position
