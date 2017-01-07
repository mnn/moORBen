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
  when (state^.options.verbose) $ io $ putStrLn x

destroyOrb :: (Int, Int) -> RuntimeStateMonad IO ()
destroyOrb (x, y) = do
  orbs %= filter fn
  return ()
    where fn (OrbState (Position fx fy) _ _) = x /= fx && y /=fy

invokePrintCharacter :: Int -> Bool -> Bool -> RuntimeStateMonad IO ()
invokePrintCharacter tapeIdx newLine keep = do
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

invokePrintInteger :: Int -> Bool -> Bool -> RuntimeStateMonad IO ()
invokePrintInteger tapeIdx newLine keep = do
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

invokePrintString :: Int -> Bool -> Bool -> RuntimeStateMonad IO ()
invokePrintString tapeIdx newLine keep = do
  when keep $ error "\"keep\" argument is not supported."
  invokePrintCharacter tapeIdx False False
  sTapes <- use tapes
  if isStackEmpty sTapes tapeIdx
    then when newLine $ io $ putStrLn ""
    else invokePrintString tapeIdx newLine keep

invokeBuiltInPocketDimension :: String -> OrbState -> RuntimeStateMonad IO Bool
invokeBuiltInPocketDimension name orbState = do
  -- TODO
  let tapeIdx = orbState^.tapeIndex
  case lookup name fns of
    Nothing -> return False
    Just f  -> r $ f tapeIdx
  where
    r x = x >> return True
    fns = [
        ("pS",   \x -> invokePrintString x True False),
        ("pSr",  \x -> invokePrintString x False False),
        ("pC",   \x -> invokePrintCharacter x True False),
        ("pCk",  \x -> invokePrintCharacter x True True),
        ("pCr",  \x -> invokePrintCharacter x False False),
        ("pCkr", \x -> invokePrintCharacter x False True),
        ("pI",   \x -> invokePrintInteger x True False),
        ("pIk",  \x -> invokePrintInteger x True True),
        ("pIr",  \x -> invokePrintInteger x False False),
        ("pIkr", \x -> invokePrintInteger x False True)
      ]

invokeUserDefinedPocketDimension :: String -> OrbState -> RuntimeStateMonad IO Bool
invokeUserDefinedPocketDimension name orbState = do
  -- TODO
  return False

invokePocketDimension :: String -> OrbState -> RuntimeStateMonad IO ()
invokePocketDimension name orbState = do
  sTapes <- use tapes
  verbosePrint $ "invokePocketDimension" ++ show orbState ++ show sTapes
  res <- invokeBuiltInPocketDimension name orbState <|> invokeUserDefinedPocketDimension name orbState
  unless res (error $ "Pocket dimension \"" ++ name ++ "\" not found.")
  return ()

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
    verbosePrint $ "new orbs: " ++ show newOrbs
    -- io $ threadDelay $ 1 * 1000 * 1000

    instructions <- instructionsBehindBalls
    mapM_ interpretInstruction instructions
    run
  return ()

io :: IO a -> RuntimeStateMonad IO a
io = liftIO

interpret :: [InterpreterFlag] -> Parser.SourceCode -> IO ()
interpret flags code = do
  let verbose = Verbose `elem` flags
  when verbose $ do
    putStrLn "Input code:"
    putStrLn $ "interpreting: " ++ show code
  let initialState = RuntimeState {
      _runtimeStateSourceCode = code
    , _runtimeStateWorld = createWorld code
    , _runtimeStateOptions = RuntimeOptions { _runtimeOptionsVerbose = verbose }
    , _runtimeStateOrbs = startingBalls code
    , _runtimeStateTapes = startingTapes
    }
  when verbose $ putStrLn $ "initialState = " ++ show initialState
  execStateT run initialState
  return ()
