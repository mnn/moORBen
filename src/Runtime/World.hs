{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Runtime.World where

import           Control.Lens
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map

import           Parser.MoorbenParser (FilePos (..), TokenWithPosition (..))
import qualified Parser.MoorbenParser as Parser

data World = World
  { _worldMap :: Map (Int, Int) TokenWithPosition
  } deriving (Show, Eq)

makeFields ''World

createWorld :: Parser.SourceCode -> World
createWorld code = World {
    _worldMap = Map.fromList $ pairs code
  } where
    pairs :: Parser.SourceCode -> [((Int, Int), TokenWithPosition)]
    pairs (Parser.SourceCode tokens) = Prelude.map tokToPair tokens
    tokToPair tokWithPos@(TokenWithPosition (FilePos x y) tok) = ((x, y), tokWithPos)
