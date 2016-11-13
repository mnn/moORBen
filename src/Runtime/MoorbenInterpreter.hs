{-# LANGUAGE TemplateHaskell #-}

module Runtime.MoorbenInterpreter (interpret, InterpreterFlag(..)) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.State

import           Parser.MoorbenParser

import           Runtime.Data.Position
import qualified Runtime.Position      as Pos

import           Runtime.Data.Velocity
import qualified Runtime.Velocity      as Vel

data InterpreterFlag = Verbose deriving (Show, Eq)

data BallState = BallState
  { _position :: Position
  , _velocity :: Velocity
  } deriving (Show)

data RuntimeOptions = RuntimeOptions
  { _verbose :: Bool
  } deriving (Show)

data RuntimeState = RuntimeState
  { _sourceCode :: SourceCode
  , _options    :: RuntimeOptions
  , _balls      :: [BallState]
  } deriving (Show)

makeLenses ''BallState
makeLenses ''RuntimeState

type RuntimeStateMonad a = StateT RuntimeState a

removeBall :: RuntimeStateMonad IO ()
removeBall = do
  x <- use balls
  io $ putStrLn $ "removeBall: balls = " ++ show x
  balls %= tail
  return ()

run :: RuntimeStateMonad IO ()
run = do
  currentBalls <- use balls
  let gotBalls = currentBalls & null & not
  when gotBalls $ do
    removeBall
    run
  return ()

io :: IO a -> RuntimeStateMonad IO a
io = liftIO

interpret :: [InterpreterFlag] -> SourceCode -> IO ()
interpret flags code = do
  let verbose = Verbose `elem` flags
  when verbose $ putStrLn "Input code:"
  putStrLn $ "TODO intepreting: " ++ show code
  let initialState = RuntimeState {
     _sourceCode = code
    , _options = RuntimeOptions { _verbose = verbose }
    , _balls = [
      BallState { _position = Position { Pos._x = 1, Pos._y = 2 }, _velocity = Velocity { Vel._x = 0, Vel._y = 1 } },
      BallState { _position = Position { Pos._x = 5, Pos._y = 3 }, _velocity = Velocity { Vel._x = 2, Vel._y = 0 } }
      ]
    }
  execStateT run initialState
  return ()
