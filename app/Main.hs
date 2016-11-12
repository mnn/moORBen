
module Main(main) where

import System.Environment
import System.Exit

import MoorbenParser

main :: IO ()
main = do
  (fName:_) <- getArgs
  putStrLn $ "File name: " ++ fName
  input <- readFile fName
  let res = parseLang input
  case res of
    Left _ -> exitFailure
    Right code -> print code -- TODO 
