module Runtime.Velocity
  ( module Runtime.Velocity
  , module Runtime.Data.Velocity
  ) where

import           Runtime.Data.Velocity

startingVelocity :: Velocity
startingVelocity = Velocity { _x = 0, _y = 0 }
