{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Arrow
import           Data.Either                   (isLeft)
import           Data.Function
import           Flow
import           Test.Framework
import           Text.ParserCombinators.Parsec hiding (spaces)

import           Parser.MoorbenParser
import qualified Runtime.Data.RuntimeState     as RtState
import           Runtime.RuntimeState

posNegCase :: Parser a -> [String -> IO ()]
posNegCase parser = [positive, negative]
  where
    fullParser = parser >> eof
    parseIt = parse fullParser ""
    positive str = assertEqual (Right ()) (parseIt str)
    negative str = assertBool (parseIt str |> isLeft)

testParser :: Parser a -> [String] -> [String] -> IO ()
testParser parser posStr negStr = do
  let [positive, negative] = posNegCase parser
  mapM_ positive posStr
  mapM_ negative negStr

test_parseWhitespace' = do
  let pos = ["", " ", "\t", "\n", "\r\n", "  \r\n  ", "   \t   \r\n  \t  \n   \r  \t "]
  let neg = ["x",  " x",  "x ",  " x ",  "\t_\t"]
  testParser whiteSpaces pos neg

test_parseString = do
  let pos =  ["\"abc\"", "\"x\"", "\" \"", "\"\""]
  let neg = ["", "a", "aaa", " w ", "\"", "\"\"\""]
  testParser mString pos neg

test_parseInt = do
  let pos = ["1", "123", "047", "11111111"]
  let neg = ["", "a1", "1a", "0w1", " 5", "4 "]
  testParser mInt pos neg

test_parseBool = do
  let pos = ["T", "F", "True", "False"]
  let neg = ["", " ", ""]
  testParser mBool pos neg

test_parsePush = do
  let pos = ["@1", "@0", "@789564", "@T", "@True", "@F", "@False"]
  let neg = ["1", "", " ", "@", "Q", "\"\""]
  testParser mPush pos neg

test_parsePop = do
  let pos = ["$", "$1", "$999"]
  let neg = ["", "$a", "a", " "]
  testParser mPop pos neg

test_parseSpike = testParser mSpike ["^"] ["", "-", "a", "0"]

test_parseOrb = testParser mOrb ["o"] ["", "-", "a", "0"]

test_parsePocketDimensionEntrance = do
  let pos = ["%a", "%pS", "%pC", "%ppPPxasldjlskajrWRWEKLJRKWEj"]
  let neg = ["", "a", "%%", "%", "aaa"]
  testParser mPortalPocketDimensionEntrance pos neg

test_operatorParser = do
  let pos = ["<", ">", "<=", ">=", "=", "!"]
  let neg = ["", "a", "_", "aaa"]
  testParser operatorParser pos neg

test_mComparator = do
  let pos = ["?<", "?>", "?<=", "?>=", "?=", "?!"]
  let neg = ["", "??", "?a", "?_", "a", "0", "aaa"]
  testParser mComparator pos neg

test_mLever = do
  let pos = ["/", "/?", "/abc", "\\", "\\?", "\\abc"]
  let neg = ["//", "\\\\", "", "_", "?", "abc"]
  testParser mLever pos neg

test_ensureTapeIndexIsValid = do
  assertEqual (Tapes 0 [TapeStack []]) startingTapes

  let tapes1 = ensureTapeIndexIsValid startingTapes 3
  let tapes1e = Tapes 0 [TapeStack [], TapeStack [], TapeStack [], TapeStack []]
  assertEqual tapes1e tapes1

  let tapes2 = ensureTapeIndexIsValid startingTapes (-2)
  let tapes2e = Tapes (-2) [TapeStack [], TapeStack [], TapeStack []]
  assertEqual tapes2e tapes2

  let tapes3 = Tapes 0 [TapeStack [StackBool True]] & \x->ensureTapeIndexIsValid x (-2) & \x->ensureTapeIndexIsValid x 1
  let tapes3e = Tapes (-2) [TapeStack [], TapeStack [], TapeStack [StackBool True], TapeStack []]
  assertEqual tapes3e tapes3

test_pushToTape = do
  let tape1 = Tapes 0 [TapeStack []]
  let sbt = StackBool True
  let si1 = StackInt 1
  let scA = StackChar 'A'
  assertEqual (Tapes 0 [TapeStack [sbt]]) $ pushToTape tape1 0 sbt
  let tape2 = Tapes 1 [TapeStack []]
  assertEqual (Tapes 1 [TapeStack [sbt]]) $ pushToTape tape2 1 sbt
  let tape3 = Tapes 1 [TapeStack [si1],TapeStack [sbt]]
  let tape3e = Tapes 1 [TapeStack [si1],TapeStack [scA, sbt]]
  assertEqual tape3e $ pushToTape tape3 2 scA

{-
test_ = do
  let pos = [""]
  let neg = [""]
  testParser _ pos neg
-}

main :: IO ()
main = htfMain htf_thisModulesTests
