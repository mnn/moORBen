{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Runtime.Velocity where

import           Control.Lens
import           Runtime.Position

data Velocity = Velocity
 { _velocityX :: Int
 , _velocityY :: Int
 } deriving (Show, Eq)

makeFields ''Velocity

startingVelocity :: Velocity
startingVelocity = Velocity { _velocityX = 0, _velocityY = 0 }
