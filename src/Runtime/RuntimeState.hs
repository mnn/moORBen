{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Runtime.RuntimeState where

import           Control.Lens
import           Data.Maybe

import           Parser.MoorbenParser
import           Runtime.OrbState
import           Runtime.RuntimeOptions
import           Runtime.World

data RuntimeState = RuntimeState
  { _runtimeStateSourceCode :: SourceCode
  , _runtimeStateOptions    :: RuntimeOptions
  , _runtimeStateOrbs       :: [OrbState]
  , _runtimeStateTapes      :: Tapes
  , _runtimeStateWorld      :: World
  } deriving (Show, Eq)

data Tapes = Tapes Int [TapeStack] deriving (Show, Eq)
data TapeStack = TapeStack [StackItem] deriving (Show, Eq)
data StackItem = StackChar Char
                | StackBool Bool
                | StackInt Int
                deriving (Show, Eq)

makeFields ''RuntimeState

startingTapes :: Tapes
startingTapes = Tapes 0 [TapeStack []]

itemsFromTapeStack :: TapeStack -> [StackItem]
itemsFromTapeStack (TapeStack items) = items

validIndexRange :: Tapes -> (Int, Int)
validIndexRange (Tapes baseIdx stacks) = (min, max) where
  min = baseIdx
  max = min + length stacks - 1

isTapeIndexValid :: Tapes -> Int -> Bool
isTapeIndexValid tapes@(Tapes baseIdx stacks) idx = idx >= min && idx <= max where
  (min, max) = validIndexRange tapes

ensureTapeIndexIsValid :: Tapes -> Int -> Tapes
ensureTapeIndexIsValid tapes@(Tapes baseIdx stacks) idx
  | isTapeIndexValid tapes idx = tapes
  | idx < min = Tapes idx (replicate (baseIdx - idx) (TapeStack []) ++ stacks)
  | otherwise = Tapes baseIdx (stacks ++ replicate (idx - baseIdx - length stacks + 1) (TapeStack []))
  where (min, max) = validIndexRange tapes

stackIndexToListIndex :: Tapes -> Int -> Int
stackIndexToListIndex (Tapes baseIdx _) stackIdx = stackIdx - baseIdx

pushToTape :: Tapes -> Int -> StackItem -> Tapes
pushToTape tapes@(Tapes baseIdx stacks) idx item = Tapes baseIdx newStacks
  where
    newStacks :: [TapeStack]
    newStacks = stacks & ix (stackIndexToListIndex tapes idx) %~ update
    update :: TapeStack -> TapeStack
    update (TapeStack items) = TapeStack (item:items)

pushStringToTape :: Tapes -> Int -> String -> Tapes
pushStringToTape tapes _ [] = tapes
pushStringToTape tapes idx xs = pushStringToTape newTapes idx (init xs)
  where newTapes = pushToTape tapes idx (StackChar $ last xs)

popFromTape :: Tapes -> Int -> (Maybe StackItem, Tapes)
popFromTape tapes@(Tapes baseIdx stacks) idx = updated --(item, Tapes baseIdx newStacks)
  where
    stack = stacks !! stackIndexToListIndex tapes idx
    items = itemsFromTapeStack stack
    item = listToMaybe items
    newStacks = stacks & ix (stackIndexToListIndex tapes idx) %~ removeHeadFromStack
    removeHeadFromStack (TapeStack (_:xs)) = TapeStack xs
    updated
      | isNothing item = (Nothing, tapes)
      | otherwise = (item, Tapes baseIdx newStacks)
