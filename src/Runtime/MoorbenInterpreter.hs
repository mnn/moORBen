module Runtime.MoorbenInterpreter (
  interpret
, InterpreterFlag(..)
) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.State

import           Utils

import qualified Parser.MoorbenParser        as Parser

import           Runtime.Data.Position
import qualified Runtime.Position            as Pos

import           Runtime.Data.Velocity
import qualified Runtime.Velocity            as Vel

import           Runtime.Data.OrbState
import qualified Runtime.OrbState            as Orb

import           Runtime.Data.RuntimeState
import qualified Runtime.RuntimeState        as RtState

import           Runtime.Data.RuntimeOptions
import qualified Runtime.RuntimeOptions      as RtOpts

data InterpreterFlag = Verbose deriving (Show, Eq)

type RuntimeStateMonad a = StateT RuntimeState a

convertPosition :: Parser.FilePos -> Position
convertPosition pos = Position { Pos._x = Parser.x pos, Pos._y = Parser.y pos }

startingBalls :: Parser.SourceCode -> [OrbState]
startingBalls (Parser.SourceCode tokens) = positions & map convertPosition & mapInd toOrbState
  where
    positions = [ pos | Parser.TokenWithPosition pos Parser.TokOrb <- tokens]
    toOrbState pos idx = OrbState {
      _position = pos
    , _velocity = Vel.startingVelocity
    , _tapeIndex = idx
    }

removeBall :: RuntimeStateMonad IO ()
removeBall = do
  x <- use orbs
  let a = head x & \x -> x^.Orb.velocity.Vel.x
  io $ putStrLn $ "removeBall: orbs = " ++ show x
  orbs %= tail
  return ()

testRun :: RuntimeStateMonad IO ()
testRun = do
  currentBalls <- use orbs
  let gotBalls = currentBalls & null & not
  when gotBalls $ do
    removeBall
    run
  return ()

interpretInstruction :: Parser.TokenWithPosition -> RuntimeStateMonad IO ()
interpretInstruction token = do
  return ()

moveOrb :: OrbState -> OrbState
moveOrb orb = OrbState {
      _position = Position { Pos._x = x, Pos._y = y }
    , _velocity = Velocity { Vel._x = velX, Vel._y = velY }
    , _tapeIndex = orb^.Orb.tapeIndex
  }
  where
    oldVelX = orb^.Orb.velocity.Vel.x
    velX = if oldVelX == 0 then 0 else oldVelX + (if oldVelX < 0 then 1 else -1)
    oldVelY = orb^.Orb.velocity.Vel.y
    velY = if oldVelY >= 0 then 1 else oldVelY - 1
    orbPos = orb^.Orb.position
    x = Pos._x orbPos + signum velX
    y = Pos._y orbPos + signum velY

run :: RuntimeStateMonad IO ()
run = do
  currentOrbs <- use orbs
  let gotOrbs = currentOrbs & null & not
  when gotOrbs $ do
    orbs %= map moveOrb

    newOrbs <- use orbs
    io $ putStrLn $ "new orbs: " ++ show newOrbs
    io $ threadDelay $ 1 * 1000 * 1000

    -- get instructions behind orbs
--     let instructions =
--     mapM_ interpretInstruction instructions
    run
  return ()

io :: IO a -> RuntimeStateMonad IO a
io = liftIO

interpret :: [InterpreterFlag] -> Parser.SourceCode -> IO ()
interpret flags code = do
  let verbose = Verbose `elem` flags
  when verbose $ putStrLn "Input code:"
  putStrLn $ "TODO intepreting: " ++ show code
  let initialState = RuntimeState {
     _sourceCode = code
    , _options = RuntimeOptions { _verbose = verbose }
    , _orbs = startingBalls code
    , _tapes = RtState.startingTapes
    }
--   execStateT testRun initialState
  execStateT run initialState
  return ()
