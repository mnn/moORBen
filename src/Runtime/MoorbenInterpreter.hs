{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Runtime.MoorbenInterpreter (
  interpret
, InterpreterFlag(..)
) where

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

destroyOrb :: (Int, Int) -> RuntimeStateMonad IO ()
destroyOrb (x, y) = do
  orbs %= filter fn
  return ()
    where
      fn (OrbState (Position fx fy) _ _) = x /= fx && y /=fy

interpretInstruction :: (Parser.TokenWithPosition, Int) -> RuntimeStateMonad IO ()
interpretInstruction (Parser.TokenWithPosition pos token, tapeIdx) = do
  -- TODO
  io $ putStrLn $ "Processing instruction " ++ show token
  case token of
    Parser.TokSpike -> destroyOrb $ Parser.filePosToPair pos
    (Parser.TokPush (Parser.TokString str)) -> tapes %= \t -> pushStringToTape t tapeIdx str
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

instructionsBehindBalls :: RuntimeStateMonad IO [(Parser.TokenWithPosition, Int)]
instructionsBehindBalls = do
  state <- get
  sWorld <- use world
  sOrbs <- use orbs
  return $ sOrbs & orbsPositions & orbPositionsToTokens sWorld
    where
      orbsPositions :: [OrbState] -> [(Position, Int)]
      orbsPositions = mapInd $ \x idx-> (view position x, idx)
      orbPositionsToTokens :: World -> [(Position, Int)] -> [(Parser.TokenWithPosition, Int)]
      orbPositionsToTokens w xs = xs & map (orbPosToToken w) & catMaybes
      orbPosToToken :: World -> (Position, Int) -> Maybe (Parser.TokenWithPosition, Int)
      orbPosToToken w (pos, idx) = fmap (\x -> (x, idx)) (Map.lookup (positionToPair pos) (w^.World.map))

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
    , _runtimeStateOptions = RuntimeOptions { _verbose = verbose }
    , _runtimeStateOrbs = startingBalls code
    , _runtimeStateTapes = startingTapes
    }
  putStrLn $ "initialState = " ++ show initialState
  execStateT run initialState
  return ()
