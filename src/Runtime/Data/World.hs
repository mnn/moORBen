{-# LANGUAGE TemplateHaskell #-}
module Runtime.Data.World where

import           Control.Lens
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Parser.MoorbenParser (TokenWithPosition)

data World = World
  { _map :: Map (Int, Int) TokenWithPosition
  } deriving (Show, Eq)

makeLenses ''World
