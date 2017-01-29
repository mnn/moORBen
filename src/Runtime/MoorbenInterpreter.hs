{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Runtime.MoorbenInterpreter (
  interpret
, InterpreterFlag(..)
) where

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.List              (intersect)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           Safe                   (headMay)
import           System.IO              (hFlush, stdout)
import           Text.Read              (readMaybe)

import qualified Parser.MoorbenParser   as Parser
import           Runtime.OrbState
import           Runtime.Position
import           Runtime.RuntimeOptions
import           Runtime.RuntimeState
import           Runtime.Velocity
import           Runtime.World          hiding (map)
import qualified Runtime.World          as World
import           Utils

data InterpreterFlag = Verbose deriving (Show, Eq)

type RuntimeStateMonad a = StateT RuntimeState a

convertPosition :: Parser.FilePos -> Position
convertPosition pos = Position { _positionX = Parser.x pos, _positionY = Parser.y pos }

startingBalls :: Parser.SourceCode -> [OrbState]
startingBalls (Parser.SourceCode tokens) = positions & map convertPosition & mapInd toOrbState
  where
    positions = [ pos | Parser.TokenWithPosition pos Parser.TokOrb <- tokens]
    toOrbState pos idx = OrbState {
      _orbStatePosition = pos
    , _orbStateVelocity = startingVelocity
    , _orbStateTapeIndex = idx
    , _orbStateReturnStack = []
    }

verbosePrint :: String -> RuntimeStateMonad IO ()
verbosePrint x = do
  state <- get
  when (state^.options.verbose) $ io $ putStrLn x

destroyOrb :: (Int, Int) -> RuntimeStateMonad IO ()
destroyOrb (x, y) = do
  orbs %= filter fn
  return ()
    where fn (OrbState (Position fx fy) _ _ _) = x /= fx && y /=fy

invokePrintCharacter :: Bool -> Bool -> Int -> RuntimeStateMonad IO ()
invokePrintCharacter newLine keep tapeIdx = do
  sTapes <- use tapes
  let (itemOpt, newTapes) = popFromTape sTapes tapeIdx
  tapes .= newTapes
  case itemOpt of
    Nothing -> error "Cannot print character - no item on stack."
    Just item -> do
      when keep $ tapes .= sTapes
      case item of
        StackChar x -> do
          io $ putStr [x]
          when newLine $ io $ putStrLn ""
        _ -> error "Cannot print character - top is not a char."

invokePrintInteger :: Bool -> Bool -> Int -> RuntimeStateMonad IO ()
invokePrintInteger newLine keep tapeIdx = do
  sTapes <- use tapes
  let (itemOpt, newTapes) = popFromTape sTapes tapeIdx
  tapes .= newTapes
  case itemOpt of
    Nothing -> error "Cannot print integer - no item on stack."
    Just item -> do
      when keep $ tapes .= sTapes
      case item of
        StackInt x -> do
          io $ putStr $ show x
          when newLine $ io $ putStrLn ""
        _ -> error "Cannot print integer - top is not an integer."

invokePrintString :: Bool -> Bool -> Int -> RuntimeStateMonad IO ()
invokePrintString newLine keep tapeIdx = do
  when keep $ error "\"keep\" argument is not supported."
  emptyAfterInvoke <- uses tapes $ \t -> isStackEmpty t tapeIdx
  unless emptyAfterInvoke $ do
    invokePrintCharacter False False tapeIdx
    sTapes <- use tapes
    if isStackEmpty sTapes tapeIdx
      then when newLine $ io $ putStrLn ""
      else invokePrintString newLine keep tapeIdx

flushStdOut :: RuntimeStateMonad IO ()
flushStdOut = io $ hFlush stdout

invokeReadInteger :: Bool -> Int -> RuntimeStateMonad IO ()
invokeReadInteger allowFail tapeIdx = do
  input <- io getLine
  case (readMaybe input :: Maybe Int) of
    Nothing  -> unless allowFail $ invokeReadInteger allowFail tapeIdx
    Just num -> tapes %= \t -> pushToTape t tapeIdx $ StackInt num

invokeReadString :: Int -> RuntimeStateMonad IO ()
invokeReadString tapeIdx = do
  input <- io getLine
  tapes %= \t -> pushStringToTape t tapeIdx input

invokeSwap :: Int -> RuntimeStateMonad IO ()
invokeSwap tapeIdx = do
  tapes' <- use tapes
  itemA <- popFromTapeOrError tapeIdx
  itemB <- popFromTapeOrError tapeIdx
  tapes %= \t -> pushToTape t tapeIdx itemA
  tapes %= \t -> pushToTape t tapeIdx itemB

invokeBuiltInPocketDimension :: String -> OrbState -> RuntimeStateMonad IO Bool
invokeBuiltInPocketDimension name orbState = do
  let tapeIdx = orbState^.tapeIndex
  case lookup name fns of
    Nothing -> return False
    Just f  -> r $ f tapeIdx
  where
    r x = x >> return True
    fns = [
        ("pS",   invokePrintString True False),
        ("pSr",  invokePrintString False False),
        ("pC",   invokePrintCharacter True False),
        ("pCk",  invokePrintCharacter True True),
        ("pCr",  invokePrintCharacter False False),
        ("pCkr", invokePrintCharacter False True),
        ("pI",   invokePrintInteger True False),
        ("pIk",  invokePrintInteger True True),
        ("pIr",  invokePrintInteger False False),
        ("pIkr", invokePrintInteger False True),
        ("rI",   invokeReadInteger False),
        ("rS",   invokeReadString),
        ("swap", invokeSwap)
      ]

invokeUserDefinedPocketDimension :: String -> OrbState -> Int -> RuntimeStateMonad IO Bool
invokeUserDefinedPocketDimension name orbState orbIndex = do
  pdStarts <- use pocketDimensionsStarts
  verbosePrint $ "Looking for custom PD: " ++ name
  case lookup name pdStarts of
    Nothing -> return False
    Just pos -> do
      zoom (orbs.ix orbIndex) $ do
        returnStack %= ((orbState^.position) :)
        position .= pos
      return True

invokePocketDimension :: String -> OrbState -> Int -> RuntimeStateMonad IO ()
invokePocketDimension name orbState orbIndex = do
  sTapes <- use tapes
  verbosePrint $ "invokePocketDimension: " ++ show orbState ++ show sTapes
  resBuiltIn <- invokeBuiltInPocketDimension name orbState
  unless resBuiltIn $ do
    resCustom <- invokeUserDefinedPocketDimension name orbState orbIndex
    unless resCustom (error $ "Pocket dimension \"" ++ name ++ "\" not found.")
  return ()

assertNumberOfItemsOnStack :: Int -> Int -> RuntimeStateMonad IO ()
assertNumberOfItemsOnStack tapeIdx expected = do
  tps <- use tapes
  when (getStackLength tps tapeIdx < expected) $ error $ "Expected " ++ show expected ++ " item(s) on stack."

assertItem :: StackItem -> (StackItem -> Bool) -> String -> RuntimeStateMonad IO ()
assertItem item test errMsg = unless (test item) $ error errMsg

assertItemIsBool :: StackItem -> RuntimeStateMonad IO Bool
assertItemIsBool i = do
  assertItem i isStackBool $ "Expected item " ++ show i ++ " to be a boolean."
  return $ case i of (StackBool x) -> x

assertItemIsInt :: StackItem -> RuntimeStateMonad IO Int
assertItemIsInt i = do
  assertItem i isStackInt $ "Expected item " ++ show i ++ " to be an integer."
  return $ case i of (StackInt x) -> x

popFromTapeOrError :: Int -> RuntimeStateMonad IO StackItem
popFromTapeOrError tapeIdx = do
  tps <- use tapes
  let (itemOpt, newTapes) = popFromTape tps tapeIdx
  tapes .= newTapes
  return $ case itemOpt of
    Nothing -> error "Attempted to pop an item from empty stack."
    Just x  -> x

interpretComparatorInts :: Int -> Int -> Parser.RelationalOperator -> Int -> RuntimeStateMonad IO Bool
interpretComparatorInts a b op tapeIdx =
  case op of
    Parser.RelLess        -> twoOp a b (<)
    Parser.RelLessOrEqual -> twoOp a b (<=)
    Parser.RelMore        -> twoOp a b (>)
    Parser.RelMoreOrEqual -> twoOp a b (>=)
    _                     -> return False
  where r x = x >> return True
        twoOp :: Int -> Int -> (Int -> Int -> Bool) -> RuntimeStateMonad IO Bool
        twoOp a b o = r $ tapes %= \t -> pushToTape t tapeIdx $ StackBool $ o a b

interpretComparatorEquals :: StackItem -> StackItem -> Parser.RelationalOperator -> Int -> RuntimeStateMonad IO Bool
interpretComparatorEquals a b op tapeIdx =
  case op of
   Parser.RelEqual -> r $ tapes %= \t -> pushToTape t tapeIdx $ StackBool $ a == b
   Parser.RelNonEqual -> r $ tapes %= \t -> pushToTape t tapeIdx $ StackBool $ a /= b
   _ -> return False
  where r x = x >> return True

interpretComparator :: Int -> Parser.RelationalOperator -> RuntimeStateMonad IO ()
interpretComparator tapeIdx op = do
  assertNumberOfItemsOnStack tapeIdx 2
  -- TODO: "if not error" after error processing is improved
  a <- popFromTapeOrError tapeIdx
  b <- popFromTapeOrError tapeIdx
  let bothInts = isStackInt a && isStackInt b
  -- TODO: "if not error" after error processing is improved
  processed1 <- if bothInts
                  then interpretComparatorInts (stackIntValue a) (stackIntValue b) op tapeIdx
                  else return False
  processed2 <- interpretComparatorEquals a b op tapeIdx
  unless (processed1 || processed2) $ error $ "Unknown operator: " ++ show op
  return ()

leverVelocity :: Int
leverVelocity = 3

getBounceSign :: Parser.LeverBounceDirection -> Int
getBounceSign Parser.BounceLeft  = -1
getBounceSign Parser.BounceRight = 1

setBounceVelocity :: Parser.LeverBounceDirection -> Int -> RuntimeStateMonad IO ()
setBounceVelocity dir orbIdx = orbs.ix orbIdx.velocity.x .= leverVelocity * getBounceSign dir

interpretLever :: Parser.LeverBounceDirection -> Int -> RuntimeStateMonad IO ()
interpretLever = setBounceVelocity

interpretTestLever :: Int -> Parser.LeverBounceDirection -> Int -> RuntimeStateMonad IO ()
interpretTestLever tapeIdx dir orbIdx = do
  p <- popFromTapeOrError tapeIdx
  -- TODO: "if not error" after error processing is improved
  b <- assertItemIsBool p
  -- TODO: "if not error" after error processing is improved
  when b $ setBounceVelocity dir orbIdx

interpretDuplicate :: Int -> Int -> Int -> RuntimeStateMonad IO ()
interpretDuplicate tapeIdx stackOffset amount = do
  let targetTapeIdx = tapeIdx + stackOffset
  (toPush, _) <- uses tapes $ \t -> popNFromTape t tapeIdx amount
  tapes %= \tps -> pushListToTape tps targetTapeIdx (reverse toPush)
  uses tapes $ \t -> verbosePrint $ "after interpretDuplicate: " ++ show t
  return ()

usePortal :: String -> Int -> OrbState -> RuntimeStateMonad IO ()
usePortal name orbIdx orbState = do
  exits <- use portalExits
  case lookup name exits of
    Nothing -> error $ "Failed to find exit of a portal named \"" ++ name ++ "\"."
    Just pos -> orbs.ix orbIdx.position .= pos

moveStackIndex :: Int -> Int -> Int -> RuntimeStateMonad IO ()
moveStackIndex offset tapeIdx orbIdx = do
  let newIndex = tapeIdx + offset
  tapes %= \t -> ensureTapeIndexIsValid t newIndex
  orbs. ix orbIdx.tapeIndex .= newIndex

pushToTapeS :: Int -> StackItem -> RuntimeStateMonad IO ()
pushToTapeS tapeIdx item = tapes %= \t -> pushToTape t tapeIdx item

pushListTapeS :: Int -> [StackItem] -> RuntimeStateMonad IO ()
pushListTapeS tapeIdx xs = tapes %= \t -> pushListToTape t tapeIdx xs

divAndRound :: Int -> Int -> Int
divAndRound x y = round $ fromIntegral x / fromIntegral y

interpretOperation :: Parser.GenericOperation -> Int -> RuntimeStateMonad IO ()
interpretOperation op tapeIdx =
  case op of
    Parser.OpAdd       -> twoIntOp (+)
    Parser.OpSubtract  -> twoIntOp (-)
    Parser.OpNegate    -> oneIntOp negate
    Parser.OpMultiply  -> twoIntOp (*)
    Parser.OpPower     -> twoIntOp (^)
    Parser.OpDivide    -> twoIntOp divAndRound
    Parser.OpIntDivide -> twoIntOp div
    Parser.OpModulo    -> twoIntOp mod
  where
    twoIntOp f = do
      a <- popFromTapeOrError tapeIdx
      b <- popFromTapeOrError tapeIdx
      [aVal, bVal] <- mapM assertItemIsInt [a, b]
      pushToTapeS tapeIdx $ StackInt (f aVal bVal)
    oneIntOp f = do
      a <- popFromTapeOrError tapeIdx
      aVal <- assertItemIsInt a
      pushToTapeS tapeIdx $ StackInt (f aVal)

interpretDupToOther :: Int -> Int -> RuntimeStateMonad IO ()
interpretDupToOther tapeIdx offset = do
  items <- uses tapes (`getStackByIndex` tapeIdx)
  pushListTapeS (tapeIdx + offset) (reverse items)

interpretPocketDimensionEnd :: Int -> RuntimeStateMonad IO ()
interpretPocketDimensionEnd orbIdx =
  zoom (orbs.ix orbIdx) $ do
    rs <- use returnStack
    case headMay rs of
      Nothing -> error "interpretPocketDimensionEnd: Return stack is empty (encountered a pocket dimension end without entering a pocket  dimension first)."
      Just pos -> do
        returnStack %= tail
        position .= pos

interpretInstruction :: (Parser.TokenWithPosition, Int, OrbState) -> RuntimeStateMonad IO ()
interpretInstruction (Parser.TokenWithPosition pos token, orbIdx, orbState) = do
  -- TODO
  verbosePrint $ "Processing instruction " ++ show token
  let tapeIdx = orbState^.tapeIndex
  case token of
    Parser.TokSpike -> destroyOrb $ Parser.filePosToPair pos
    (Parser.TokPush (Parser.TokString str)) -> tapes %= \t -> pushStringToTape t tapeIdx str
    (Parser.TokPush (Parser.TokInt int)) -> tapes %= \t -> pushToTape t tapeIdx $ StackInt int
    (Parser.TokPush Parser.TokTrue) -> tapes %= \t -> pushToTape t tapeIdx $ StackBool True
    (Parser.TokPush Parser.TokFalse) -> tapes %= \t -> pushToTape t tapeIdx $ StackBool False
    (Parser.TokPocketDimensionEntrance name) -> invokePocketDimension name orbState orbIdx
    (Parser.TokComparator op) -> interpretComparator tapeIdx op
    (Parser.TokLeverStatic dir) -> interpretLever dir orbIdx
    (Parser.TokLeverTest dir) -> interpretTestLever tapeIdx dir orbIdx
    Parser.TokOrb -> return ()
    (Parser.TokDuplicate stackOffset amount) -> interpretDuplicate tapeIdx stackOffset amount
    (Parser.TokPortalExit _) -> return ()
    (Parser.TokPortalEntrance name) -> usePortal name orbIdx orbState
    (Parser.TokPortalTwoWay name) -> usePortal name orbIdx orbState
    (Parser.TokMoveStackIndex offset) -> moveStackIndex offset tapeIdx orbIdx
    (Parser.TokOperation op) -> interpretOperation op tapeIdx
    (Parser.TokPop c) -> tapes %= \t -> snd $ popNFromTape t tapeIdx c
    (Parser.TokDuplicateToOther offset) -> interpretDupToOther tapeIdx offset
    Parser.TokPocketDimensionEnd -> interpretPocketDimensionEnd orbIdx
    _ -> error $ "Unknown instruction: " ++ show token
  flushStdOut

moveOrb :: OrbState -> OrbState
moveOrb orb = OrbState {
      _orbStatePosition = Position { _positionX = pX, _positionY = pY }
    , _orbStateVelocity = Velocity { _velocityX = velX, _velocityY = velY }
    , _orbStateTapeIndex = orb^.tapeIndex
    , _orbStateReturnStack = orb^.returnStack
  }
  where
    vel = orb^.velocity
    oldVelX = vel^.x
    velX = if oldVelX == 0 then 0 else oldVelX + (if oldVelX < 0 then 1 else -1)
    oldVelY = orb^.velocity.y
    velY = if oldVelY >= 0 then 1 else oldVelY - 1
    orbPos = orb^.position
    pX = orbPos^.x + signum oldVelX
    pY = orbPos^.y + if oldVelX == 0 then signum oldVelY else 0

instructionsBehindBalls :: RuntimeStateMonad IO [(Parser.TokenWithPosition, Int, OrbState)]
instructionsBehindBalls = do
  state <- get
  sWorld <- use world
  sOrbs <- use orbs
  return $ sOrbs & orbsPositions & orbPositionsToTokens sWorld
    where
      orbsPositions :: [OrbState] -> [(Position, Int, OrbState)]
      orbsPositions = mapInd $ \x idx-> (view position x, idx, x)
      orbPositionsToTokens :: World -> [(Position, Int, OrbState)] -> [(Parser.TokenWithPosition, Int, OrbState)]
      orbPositionsToTokens w xs = xs & map (orbPosToToken w) & catMaybes
      orbPosToToken :: World -> (Position, Int, OrbState) -> Maybe (Parser.TokenWithPosition, Int, OrbState)
      orbPosToToken w (pos, idx, orbState) = fmap (\x -> (x, idx, orbState)) (Map.lookup (positionToPair pos) (w^.World.map))

genPortalExitsMap :: Parser.SourceCode -> [(String, Position)]
genPortalExitsMap (Parser.SourceCode tokens) = exitsCombined & map conv where
  simpleExits = [ (name, pos) | Parser.TokenWithPosition pos (Parser.TokPortalExit name) <- tokens]
  twoWays = [ (name, pos) | Parser.TokenWithPosition pos (Parser.TokPortalTwoWay name) <- tokens]
  exitsCombined = simpleExits ++  twoWays
  conv (name, pos) = (name, convertPosition pos)

genPocketDimensionsStartsMap :: Parser.SourceCode -> [(String, Position)]
genPocketDimensionsStartsMap (Parser.SourceCode tokens) = starts & map conv where
  starts = [ (name, pos) | Parser.TokenWithPosition pos (Parser.TokPocketDimensionStart name) <- tokens]
  conv (name, pos) = (name, convertPosition pos)

validateState :: RuntimeState -> IO ()
validateState state = do
  let nameConflicts = getNames (state^.portalExits) `intersect` getNames (state^.pocketDimensionsStarts)
  unless (null nameConflicts) $ error $ "Found ID conflicts (portals, pocket dimensions): " ++ show nameConflicts
  return ()
  where getNames = map fst

run :: RuntimeStateMonad IO ()
run = do
  gotOrbs <- fmap (not . null) (use orbs)
  when gotOrbs $ do
    orbs %= map moveOrb

    newOrbs <- use orbs
    verbosePrint $ "new orbs: " ++ show newOrbs
--     io $ threadDelay $ 1 * 1000 * 500

    instructions <- instructionsBehindBalls
    mapM_ interpretInstruction instructions
    run
  return ()

io :: IO a -> RuntimeStateMonad IO a
io = liftIO

interpret :: [InterpreterFlag] -> Parser.SourceCode -> IO ()
interpret flags code@(Parser.SourceCode tokens) = do
  let verbose = Verbose `elem` flags
  when verbose $ do
    putStrLn "interpreting: "
    mapM_ print tokens
  let initialState = RuntimeState {
      _runtimeStateSourceCode = code
    , _runtimeStateWorld = createWorld code
    , _runtimeStateOptions = RuntimeOptions { _runtimeOptionsVerbose = verbose }
    , _runtimeStateOrbs = startingBalls code
    , _runtimeStateTapes = startingTapes
    , _runtimeStatePortalExits = genPortalExitsMap code
    , _runtimeStatePocketDimensionsStarts = genPocketDimensionsStartsMap code
    }
  when verbose $ putStrLn $ "initialState = " ++ show initialState
  validateState initialState
  execStateT run initialState
  return ()
