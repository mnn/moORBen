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
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe

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
    }

verbosePrint :: String -> RuntimeStateMonad IO ()
verbosePrint x = do
  state <- get
  when (state^.options.verbose) (io $ putStrLn x)

destroyOrb :: (Int, Int) -> RuntimeStateMonad IO ()
destroyOrb (x, y) = do
  orbs %= filter fn
  return ()
    where fn (OrbState (Position fx fy) _ _) = x /= fx && y /=fy

invokePrintCharacter :: Int -> RuntimeStateMonad IO ()
invokePrintCharacter tapeIdx = do
  sTapes <- use tapes
  let (itemOpt, newTapes) = popFromTape sTapes tapeIdx
  tapes .= newTapes
  case itemOpt of
    Nothing -> error "Cannot print character - no item on stack."
    Just item -> case item of
      StackChar x -> io $ putStr [x]
      _ -> error "Cannot print character - top is not a char."

invokePrintString :: Int -> RuntimeStateMonad IO ()
invokePrintString tapeIdx = do
  invokePrintCharacter tapeIdx
  sTapes <- use tapes
  if isStackEmpty sTapes tapeIdx
    then io $ putStrLn ""
    else invokePrintString tapeIdx

invokeBuiltInPocketDimension :: String -> OrbState -> RuntimeStateMonad IO Bool
invokeBuiltInPocketDimension name orbState = do
  -- TODO
  let tapeIdx = orbState^.tapeIndex
  case name of
    "pS" -> do
      invokePrintString tapeIdx
      return True
    "pC" -> do
      invokePrintCharacter tapeIdx
      return True
    _ -> return False

invokeUserDefinedPocketDimension :: String -> OrbState -> RuntimeStateMonad IO Bool
invokeUserDefinedPocketDimension name orbState = do
  -- TODO
  return False

invokePocketDimension :: String -> OrbState -> RuntimeStateMonad IO ()
invokePocketDimension name orbState = do
  sTapes <- use tapes
  io $ putStrLn $ "invokePocketDimension" ++ show orbState ++ show sTapes
  res <- invokeBuiltInPocketDimension name orbState <|> invokeUserDefinedPocketDimension name orbState
  unless res (error $ "Pocket dimension \"" ++ name ++ "\" not found.")
  return ()

interpretInstruction :: (Parser.TokenWithPosition, Int, OrbState) -> RuntimeStateMonad IO ()
interpretInstruction (Parser.TokenWithPosition pos token, orbIdx, orbState) = do
  -- TODO
  io $ putStrLn $ "Processing instruction " ++ show token
  let tapeIdx = orbState^.tapeIndex
  case token of
    Parser.TokSpike -> destroyOrb $ Parser.filePosToPair pos
    (Parser.TokPush (Parser.TokString str)) -> tapes %= \t -> pushStringToTape t tapeIdx str
    (Parser.TokPocketDimensionEntrance name) -> invokePocketDimension name orbState
    _ -> return ()
  return ()

moveOrb :: OrbState -> OrbState
moveOrb orb = OrbState {
      _orbStatePosition = Position { _positionX = pX, _positionY = pY }
    , _orbStateVelocity = Velocity { _velocityX = velX, _velocityY = velY }
    , _orbStateTapeIndex = orb^.tapeIndex
  }
  where
    vel = orb^.velocity
    oldVelX = vel^.x
    velX = if oldVelX == 0 then 0 else oldVelX + (if oldVelX < 0 then 1 else -1)
    oldVelY = orb^.velocity.y
    velY = if oldVelY >= 0 then 1 else oldVelY - 1
    orbPos = orb^.position
    pX = orbPos^.x + signum velX
    pY = orbPos^.y + signum velY

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

run :: RuntimeStateMonad IO ()
run = do
  gotOrbs <- fmap (not . null) (use orbs)
  when gotOrbs $ do
    orbs %= map moveOrb

    newOrbs <- use orbs
    io $ putStrLn $ "new orbs: " ++ show newOrbs
    io $ threadDelay $ 1 * 1000 * 1000

    instructions <- instructionsBehindBalls
    mapM_ interpretInstruction instructions
    run
  return ()

io :: IO a -> RuntimeStateMonad IO a
io = liftIO

interpret :: [InterpreterFlag] -> Parser.SourceCode -> IO ()
interpret flags code = do
  let verbose = Verbose `elem` flags
  when verbose $ putStrLn "Input code:"
  putStrLn $ "TODO interpreting: " ++ show code
  let initialState = RuntimeState {
      _runtimeStateSourceCode = code
    , _runtimeStateWorld = createWorld code
    , _runtimeStateOptions = RuntimeOptions { _runtimeOptionsVerbose = verbose }
    , _runtimeStateOrbs = startingBalls code
    , _runtimeStateTapes = startingTapes
    }
  putStrLn $ "initialState = " ++ show initialState
  execStateT run initialState
  return ()
