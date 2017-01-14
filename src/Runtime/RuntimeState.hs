{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Runtime.RuntimeState where

import           Control.Lens
import           Data.Maybe
import           Data.Tuple             (swap)

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
pushToTape tapes@(Tapes baseIdx stacks) idx item = Tapes baseIdx newStacks where
  newStacks :: [TapeStack]
  newStacks = stacks & ix (stackIndexToListIndex tapes idx) %~ update
  update :: TapeStack -> TapeStack
  update (TapeStack items) = TapeStack (item:items)

pushListToTape :: Tapes -> Int -> [StackItem] -> Tapes
pushListToTape tapes@(Tapes baseIdx stacks) idx toPush = newTapes where
  newTapes = foldl f tapes toPush
  f acc = pushToTape acc idx

pushStringToTape :: Tapes -> Int -> String -> Tapes
pushStringToTape tapes _ [] = tapes
pushStringToTape tapes idx xs = pushStringToTape newTapes idx (init xs)
  where newTapes = pushToTape tapes idx (StackChar $ last xs)

popFromTape :: Tapes -> Int -> (Maybe StackItem, Tapes)
popFromTape tapes@(Tapes baseIdx stacks) idx = updated where
  stack = stacks !! stackIndexToListIndex tapes idx
  items = itemsFromTapeStack stack
  item = listToMaybe items
  newStacks = stacks & ix (stackIndexToListIndex tapes idx) %~ removeHeadFromStack
  removeHeadFromStack (TapeStack (_:xs)) = TapeStack xs
  updated
    | isNothing item = (Nothing, tapes)
    | otherwise = (item, Tapes baseIdx newStacks)

popNFromTape :: Tapes -> Int -> Int -> ([StackItem], Tapes)
popNFromTape tapes@(Tapes baseIdx stacks) idx n = res where
  res = swap $ foldl f (tapes, []) [1..n]
  f (tps, res) _ = let
    (itemOpt, newTps) = popFromTape tps idx
    newRes = res ++ maybeToList itemOpt
      in (newTps, newRes)

isStackEmpty :: Tapes -> Int -> Bool
isStackEmpty tapes idx = getStackLength tapes idx == 0

getStackLength :: Tapes -> Int -> Int
getStackLength tapes@(Tapes baseIdx stacks) idx =
  stackToList stack & length
  where
    stack = stacks !! stackIndexToListIndex tapes idx
    stackToList (TapeStack x) = x

isStackChar :: StackItem -> Bool
isStackChar (StackChar _) = True
isStackChar _             = False

isStackInt :: StackItem -> Bool
isStackInt (StackInt _) = True
isStackInt _            = False

isStackBool :: StackItem -> Bool
isStackBool (StackBool _) = True
isStackBool _             = False
