module Runtime.Position
  ( module Runtime.Position
  , module Runtime.Data.Position
  ) where

import           Runtime.Data.Position

positionToPair :: Position -> (Int, Int)
positionToPair (Position x y) = (x, y)
