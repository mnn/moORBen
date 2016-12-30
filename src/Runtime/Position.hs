{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Runtime.Position where

import           Control.Lens

data Position = Position
  { _positionX :: Int
  , _positionY :: Int
  } deriving (Show, Eq)

makeFields ''Position

positionToPair :: Position -> (Int, Int)
positionToPair (Position x y) = (x, y)
