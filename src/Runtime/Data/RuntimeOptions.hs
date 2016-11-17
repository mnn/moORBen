{-# LANGUAGE TemplateHaskell #-}
module Runtime.Data.RuntimeOptions where

import           Control.Lens

data RuntimeOptions = RuntimeOptions
  { _verbose :: Bool
  } deriving (Show, Eq)

makeLenses ''RuntimeOptions
