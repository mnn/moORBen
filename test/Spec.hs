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

test_parseWhitespace' :: IO ()
test_parseWhitespace' = do
  let pos = ["", " ", "\t", "\n", "\r\n", "  \r\n  ", "   \t   \r\n  \t  \n   \r  \t "]
  let neg = ["x",  " x",  "x ",  " x ",  "\t_\t"]
  testParser whiteSpaces pos neg

test_parseString :: IO ()
test_parseString = do
  let pos =  ["\"abc\"", "\"x\"", "\" \"", "\"\""]
  let neg = ["", "a", "aaa", " w ", "\"", "\"\"\""]
  testParser mString pos neg

test_parseInt :: IO ()
test_parseInt = do
  let pos = ["1", "123", "047", "11111111", "-1"]
  let neg = ["", "a1", "1a", "0w1", " 5", "4 ", "--1"]
  testParser mInt pos neg

test_parseBool :: IO ()
test_parseBool = do
  let pos = ["T", "F", "True", "False"]
  let neg = ["", " ", ""]
  testParser mBool pos neg

test_parsePush :: IO ()
test_parsePush = do
  let pos = ["@1", "@0", "@789564", "@T", "@True", "@F", "@False", "@-1"]
  let neg = ["1", "", " ", "@", "Q", "\"\""]
  testParser mPush pos neg

test_parsePop :: IO ()
test_parsePop = do
  let pos = ["$", "$1", "$999"]
  let neg = ["", "$a", "a", " "]
  testParser mPop pos neg

test_parseSpike :: IO ()
test_parseSpike = testParser mSpike ["^"] ["", "-", "a", "0"]

test_parseOrb :: IO ()
test_parseOrb = testParser mOrb ["o"] ["", "-", "a", "0"]

test_parsePocketDimensionEntrance :: IO ()
test_parsePocketDimensionEntrance = do
  let pos = ["%a", "%pS", "%pC", "%ppPPxasldjlskajrWRWEKLJRKWEj"]
  let neg = ["", "a", "%%", "%", "aaa"]
  testParser mPortalPocketDimensionEntrance pos neg

test_operatorParser :: IO ()
test_operatorParser = do
  let pos = ["<", ">", "<=", ">=", "=", "!"]
  let neg = ["", "a", "_", "aaa"]
  testParser operatorParser pos neg

test_mComparator :: IO ()
test_mComparator = do
  let pos = ["?<", "?>", "?<=", "?>=", "?=", "?!"]
  let neg = ["", "??", "?a", "?_", "a", "0", "aaa"]
  testParser mComparator pos neg

test_mLever :: IO ()
test_mLever = do
  let pos = ["/", "/?", "/abc", "\\", "\\?", "\\abc"]
  let neg = ["//", "\\\\", "", "_", "?", "abc"]
  testParser mLever pos neg

test_ensureTapeIndexIsValid :: IO ()
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


test_pushToTape :: IO ()
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

  let tape4 = Tapes 0 [TapeStack []]
  let tape4e = Tapes 0 [TapeStack [StackChar 'A', StackChar 'B']]
  assertEqual tape4e $ pushStringToTape tape4 0 "AB"


test_popFromTape :: IO ()
test_popFromTape = do
  let sbt = StackBool True
  let si1 = StackInt 1
  let scA = StackChar 'A'

  let exp1 = (Nothing, Tapes 0 [TapeStack []])
  let tape1 = Tapes 0 [TapeStack []]
  assertEqual exp1 $ popFromTape tape1 0

  let exp2 = (Just sbt, Tapes 0 [TapeStack []])
  let tape2 = Tapes 0 [TapeStack [sbt]]
  assertEqual exp2 $ popFromTape tape2 0

  let exp3 = (Just sbt, Tapes 0 [TapeStack [scA]])
  let tape3 = Tapes 0 [TapeStack [sbt, scA]]
  assertEqual exp3 $ popFromTape tape3 0

  let exp4 = (Just sbt, Tapes 0 [TapeStack [scA], TapeStack[si1]])
  let tape4 = Tapes 0 [TapeStack [scA], TapeStack [sbt, si1]]
  assertEqual exp4 $ popFromTape tape4 1

  let exp5 = (Just sbt, Tapes (-1) [TapeStack [scA], TapeStack[si1]])
  let tape5 = Tapes (-1) [TapeStack [scA], TapeStack [sbt, si1]]
  assertEqual exp5 $ popFromTape tape5 0

test_popNFromTape :: IO ()
test_popNFromTape = do
  let sbt = StackBool True
  let si1 = StackInt 1
  let scA = StackChar 'A'

  let exp1 = ([], Tapes 0 [TapeStack []])
  let tape1 = Tapes 0 [TapeStack []]
  assertEqual exp1 $ popNFromTape tape1 0 0

  let exp2 = ([sbt, si1, scA], Tapes 0 [TapeStack []])
  let tape2 = Tapes 0 [TapeStack [sbt, si1, scA]]
  assertEqual exp2 $ popNFromTape tape2 0 3

  let exp3 = ([sbt, si1], Tapes 0 [TapeStack [scA]])
  let tape3 = Tapes 0 [TapeStack [sbt, si1, scA]]
  assertEqual exp3 $ popNFromTape tape3 0 2

test_pushListToTape :: IO ()
test_pushListToTape = do
  let sbt = StackBool True
  let si1 = StackInt 1
  let si2 = StackInt 2
  let scA = StackChar 'A'

  let exp1 = Tapes 0 [TapeStack [sbt, si1, scA, si2]]
  let tape1 = Tapes 0 [TapeStack [si2]]
  assertEqual exp1 $ pushListToTape tape1 0 [scA, si1, sbt]

test_mComment :: IO ()
test_mComment = do
  let pos = ["`abc", "`", "``", "`\n"]
  let neg = ["", "x `","\n", "\n`"]
  testParser mComment pos neg

test_mPortalTwoWay :: IO ()
test_mPortalTwoWay = do
  let pos = ["-A", "-abc", "-c"]
  let neg = ["-", "", "- ", "--", "-1", "abc", "_A", "~A"]
  testParser mPortalTwoWay pos neg

test_mPortalEntrance :: IO ()
test_mPortalEntrance = do
  let pos = ["_A", "_abc", "_c"]
  let neg = ["_", "", "_ ", "__", "_1", "abc", "-A", "~A"]
  testParser mPortalEntrance pos neg

test_mPortalExit :: IO ()
test_mPortalExit = do
  let pos = ["~A", "~abc", "~c"]
  let neg = ["~", "", "~ ", "~~", "~1", "abc", "-A", "_A"]
  testParser mPortalExit pos neg

test_mDuplicateNItems :: IO ()
test_mDuplicateNItems = do
  let pos = ["=", "={", "=}", "=3", "=2{1", "={1"]
  let neg = ["==", "{", "}=", "", " =", "1=", "={{", "=-1", "={-1"]
  testParser mDuplicateNItems pos neg

test_mMoveStackIndex :: IO ()
test_mMoveStackIndex = do
  let pos = ["}", "{", "{2", "}3"]
  let neg = ["", "}}", "{{", "{-1"]
  testParser mMoveStackIndex pos neg

test_mOperation :: IO ()
test_mOperation = do
  let pos = [":+", ":-", ":!", ":*", ":^", ":/", ":\\", ":\\\\"]
  let neg = ["", "::", "+", "\\"]
  testParser mOperation pos neg

{-
test_ = do
  let pos = [""]
  let neg = [""]
  testParser _ pos neg
-}

main :: IO ()
main = htfMain htf_thisModulesTests
