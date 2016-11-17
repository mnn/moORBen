module Runtime.RuntimeState
  ( module Runtime.RuntimeState
  , module Runtime.Data.RuntimeState
  ) where

import           Runtime.Data.RuntimeState

startingTapes :: Tapes
startingTapes = Tapes 0 [TapeStack []]

validIndexRange :: Tapes -> (Int, Int)
validIndexRange (Tapes baseIdx stacks) = (min, max) where
  min = baseIdx
  max = min + length stacks - 1

isTypeIndexValid :: Tapes -> Int -> Bool
isTypeIndexValid tapes@(Tapes baseIdx stacks) idx = idx >= min && idx <= max where
  (min, max) = validIndexRange tapes

ensureTapeIndexIsValid :: Tapes -> Int -> Tapes
ensureTapeIndexIsValid tapes@(Tapes baseIdx stacks) idx
  | isTypeIndexValid tapes idx = tapes
  | idx < min = Tapes idx (replicate (baseIdx - idx) (TapeStack []) ++ stacks)
  | otherwise = Tapes baseIdx (stacks ++ replicate (idx - baseIdx - length stacks + 1) (TapeStack []))
  where (min, max) = validIndexRange tapes
