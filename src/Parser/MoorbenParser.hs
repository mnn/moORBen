module Parser.MoorbenParser where

import           Control.Arrow                 (first)
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.List.Utils
import           Data.Maybe
import           Flow
import           Text.Parsec.Pos
import           Text.ParserCombinators.Parsec hiding (spaces)

data GenericOperation = OpAdd
                      | OpSubtract
                      | OpNegate
                      | OpMultiply
                      | OpPower
                      | OpDivide
                      | OpIntDivide
                      | OpModulo
                        deriving (Show, Eq)
data RelationalOperator = RelLess
                        | RelMore
                        | RelMoreOrEqual
                        | RelLessOrEqual
                        | RelEqual
                        | RelNonEqual
                          deriving (Show, Eq)
data LeverBounceDirection = BounceLeft | BounceRight deriving (Show, Eq)
data Token = TokString String
           | TokInt Int
           | TokPush Token
           | TokPop Int
           | TokTrue
           | TokFalse
           | TokSpike
           | TokOrb
           | TokPocketDimensionEntrance String
           | TokPocketDimensionStart String
           | TokPocketDimensionEnd
           | TokPortalTwoWay String
           | TokPortalEntrance String
           | TokPortalExit String
           | TokComparator RelationalOperator
           | TokOperation GenericOperation
           | TokLeverStatic LeverBounceDirection
           | TokLeverTest LeverBounceDirection
           | TokLeverRemote LeverBounceDirection String
           | TokDuplicate Int Int
           | TokDuplicateToOther Int
           | TokComment String
           | TokMoveStackIndex Int
           deriving (Show, Eq)
data FilePos = FilePos { x :: Int, y :: Int } deriving (Show, Eq)
data TokenWithPosition = TokenWithPosition FilePos Token deriving (Show, Eq)
data SourceCode = SourceCode [TokenWithPosition] deriving (Show, Eq)

filePosToPair :: FilePos -> (Int, Int)
filePosToPair (FilePos x y) = (x, y)

whiteSpaces :: Parser ()
whiteSpaces = skipMany $ space <|> char '\n'

wrapWithPosition ::  Parser Token -> Parser TokenWithPosition
wrapWithPosition p = do
  rawPos <- getPosition
  let pos = FilePos (sourceColumn rawPos) (sourceLine rawPos)
  x <- p
  return $ TokenWithPosition pos x

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf"
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

unescapeString :: String -> String
unescapeString input = foldl f input replaces where
  f acc (x, y) = replace x y acc
  replaces = [("\\", "\\")
             ,("0", "\\0")
             ,("n", "\\n")
             ,("r", "\\r")
             ,("v", "\\v")
             ,("t", "\\t")
             ,("b", "\\b")
             ,("f", "\\f")
             ] & map (first ("\\" ++))

stringCharacter :: Parser String
stringCharacter = fmap return nonEscape <|> escape

natParser :: Parser Int
natParser = do
  d <- many1 digit
  return $ read d

intParser :: Parser Int
intParser = do
  sign <- option "" $ string "-"
  d <- many1 digit
  return $ read $ sign ++ d

idParser :: Parser String
idParser = do
  f <- letter
  r <- many alphaNum
  return $ f:r

mString :: Parser Token
mString = do
  char '"'
  str <- many stringCharacter
  char '"'
  return $ TokString $ concat str & unescapeString

mInt :: Parser Token
mInt = do
  d <- intParser
  return $ TokInt d

mBool :: Parser Token
mBool = do
  x <- try (string "True") <|> try (string "False") <|> string "T" <|> string "F"
  return $ case x of
    x | x `elem` ["T", "True"] -> TokTrue
    _ -> TokFalse

mPush :: Parser Token
mPush = do
  char '@'
  inner <- mBool <|> mString <|> mInt
  return $ TokPush inner

extractInt :: Token -> Int
extractInt (TokInt a) = a

mPop :: Parser Token
mPop = do
  char '$'
  num <- option 1 intParser
  return $ TokPop num

mSpike :: Parser Token
mSpike = do
  char '^'
  return TokSpike

mOrb :: Parser Token
mOrb = do
  char 'o'
  return TokOrb


mPortalPocketDimensionEntrance :: Parser Token
mPortalPocketDimensionEntrance = do
  char '%'
  id <- idParser
  return $ TokPocketDimensionEntrance id

operatorParser :: Parser RelationalOperator
operatorParser = foldl1 (<|>) parsers
  where
    strToRelOp :: [(String, RelationalOperator)]
    strToRelOp = [ ("<=", RelLessOrEqual), (">=", RelMoreOrEqual), ("<", RelLess), (">", RelMore),("=", RelEqual), ("!", RelNonEqual)]
    pairToParser :: (String, RelationalOperator) -> Parser RelationalOperator
    pairToParser (str, op) = try (string str) >> return op
    parsers :: [Parser RelationalOperator]
    parsers = map pairToParser strToRelOp

mComparator :: Parser Token
mComparator = do
  char '?'
  op <- operatorParser
  return $ TokComparator op

mLever :: Parser Token
mLever = do
  dir <- (char '/' >> return BounceLeft) <|> (char '\\' >> return BounceRight)
  test <- optionMaybe $ char '?'
  remote <- case test of Nothing -> optionMaybe idParser
                         Just _  -> return Nothing
  return $ case (test, remote) of (Nothing, Nothing) -> TokLeverStatic dir
                                  (Just _, _)        -> TokLeverTest dir
                                  (Nothing, Just id) -> TokLeverRemote dir id

mComment :: Parser Token
mComment = do
  char '`'
  str <- manyTill anyChar (void newline <|> eof)
  return $ TokComment str

mPortalTwoWay :: Parser Token
mPortalTwoWay = do
  char '-'
  id <- idParser
  return $ TokPortalTwoWay id

mPortalEntrance :: Parser Token
mPortalEntrance = do
  char '_'
  id <- idParser
  return $ TokPortalEntrance id

mPortalExit :: Parser Token
mPortalExit = do
  char '~'
  id <- idParser
  return $ TokPortalExit id

calcDirCoef :: Maybe Char -> Int
calcDirCoef (Just '{') = -1
calcDirCoef (Just '}') = 1
calcDirCoef Nothing    = 0

mDuplicateNItems :: Parser Token
mDuplicateNItems = do
  char '='
  amount <- option 1 natParser
  dirChar <- optionMaybe $ char '{' <|> char '}'
  stackOffsetRaw <- option 0 natParser
  let stackOffset = stackOffsetRaw * calcDirCoef dirChar
  return $ TokDuplicate stackOffset amount

dirCharParser :: Parser Char
dirCharParser = char '{' <|> char '}'

mMoveStackIndex :: Parser Token
mMoveStackIndex = do
  dirChar <- dirCharParser
  amount <- option 1 natParser
  let off = amount * calcDirCoef (Just dirChar)
  return $ TokMoveStackIndex off

parseGenericOperationSymbol :: Parser GenericOperation
parseGenericOperationSymbol = do
  let ops = [ ("+", OpAdd)
            , ("-", OpSubtract)
            , ("!", OpNegate)
            , ("*", OpMultiply)
            , ("^", OpPower)
            , ("/", OpDivide)
            ]
  let opParsers = map opToParser ops
  let modParser = opToParser ("\\\\", OpModulo)
  let intDivParser = opToParser ("\\", OpIntDivide)
  foldl1 (<|>) opParsers <|> try modParser <|> intDivParser
  where
    opToParser :: (String, GenericOperation) -> Parser GenericOperation
    opToParser (str, res) = do
      string str
      return res

mOperation :: Parser Token
mOperation = do
  char ':'
  op <- parseGenericOperationSymbol
  return $ TokOperation op

-- Phase 5
mPushWholeStack :: Parser Token
mPushWholeStack = do
  string "=="
  dirChar <- dirCharParser
  amount <- option 1 natParser
  let off = amount * calcDirCoef (Just dirChar)
  return $ TokDuplicateToOther off

mPortalPocketDimensionStart :: Parser Token
mPortalPocketDimensionStart = do
  string "~%"
  id <- idParser
  return $ TokPocketDimensionStart id

mPortalPocketDimensionEnd :: Parser Token
mPortalPocketDimensionEnd = do
  string "_%"
  return TokPocketDimensionEnd

-- Phase 6
-- holder
-- sensor

mLang :: Parser SourceCode
mLang = do
  whiteSpaces
  let rawParsers = [
                    mPush, mPop, mSpike, mOrb, mPortalPocketDimensionEntrance, mComparator, mLever, mComment,
                    mPortalTwoWay, mMoveStackIndex, mOperation,
                    try mPushWholeStack <|> mDuplicateNItems, try mPortalPocketDimensionStart <|> mPortalExit,
                    try mPortalPocketDimensionEnd <|> mPortalEntrance
                   ]
  let rawParsersWrapped = map wrapWithPosition rawParsers
  let highParsers = [] :: [Parser TokenWithPosition]
  let allParsers = rawParsersWrapped ++ highParsers
  let exprParser = foldl1 (<|>) allParsers
  x <- sepEndBy exprParser whiteSpaces
  eof
  return $ SourceCode x

parseLang :: String -> Either ParseError SourceCode
parseLang = parse mLang "moORBen"
