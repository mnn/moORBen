module Runtime.World
  ( module Runtime.World
  , module Runtime.Data.World
  ) where

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map

import           Parser.MoorbenParser (TokenWithPosition(..), FilePos (..))
import qualified Parser.MoorbenParser as Parser
import           Runtime.Data.World
import qualified Runtime.Data.World as W

createWorld :: Parser.SourceCode -> World
createWorld code = W.World {
    W._map = Map.fromList $ pairs code
  } where
    pairs :: Parser.SourceCode -> [((Int, Int), TokenWithPosition)]
    pairs (Parser.SourceCode tokens) = Prelude.map tokToPair tokens
    tokToPair tokWithPos@(TokenWithPosition (FilePos x y) tok) = ((x, y), tokWithPos)
