{-# LANGUAGE MultiWayIf #-}

module Main(main) where

import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Flow
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Parser.MoorbenParser
import qualified Runtime.MoorbenInterpreter as IP

data MainFlag = Verbose | Version deriving (Show, Eq)

options :: [OptDescr MainFlag]
options =
  [ Option ['v'] ["verbose"] (NoArg Verbose) "more verbose outputs"
  , Option ['V', '?'] ["version"] (NoArg Version) "show version number"
  ]

opts :: [String] -> IO ([MainFlag], [String])
opts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (o,n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: mORBen [OPTION...] files..."

interpreterFlags :: ([MainFlag], [String]) -> [IP.InterpreterFlag]
interpreterFlags options = concatMap mainFlagToInterpreterFlagOpt (fst options)
  where
    mapping = [(Verbose, IP.Verbose)]
    mainFlagToInterpreterFlagOpt :: MainFlag -> [IP.InterpreterFlag]
    mainFlagToInterpreterFlagOpt flag = lookup flag mapping |> maybeToList

crashWithMessage :: String -> IO ()
crashWithMessage msg = do
  hPutStrLn stderr msg
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  options <- opts args
  let flags = options |> fst
  let verbose = flags |> elem Verbose
  let showVersionFlag = flags |> elem Version
  if | showVersionFlag -> putStrLn "0.0.1"
     | otherwise       -> do
      let notMatched = options |> snd
      when (null notMatched) $ crashWithMessage "Missing file name."
      let fName = notMatched |> head
      when verbose $ putStrLn $ "Input file name: " ++ fName
      input <- readFile fName
      when verbose $ putStrLn $ "File contents: \n" ++ input
      let res = parseLang input
      case res of
        Left err   -> crashWithMessage $ "Failed to parse input file.\n" ++ show err
        Right code -> IP.interpret (interpreterFlags options) code
