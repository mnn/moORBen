module Parser.MoorbenParser where

import           Data.Function
import           Data.List
import           Data.Maybe
import           Flow
import           Text.Parsec.Pos
import           Text.ParserCombinators.Parsec hiding (spaces)

data GenericOperation = OpAdd deriving (Show, Eq)
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
           | TokDuplicate Int
           | TokDuplicateToOther Int
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

stringCharacter :: Parser String
stringCharacter = fmap return nonEscape <|> escape

intParser :: Parser Int
intParser = do
  d <- many1 digit
  return $ read d

idParser :: Parser String
idParser = many1 letter

mString :: Parser Token
mString = do
  char '"'
  str <- many stringCharacter
  char '"'
  return $ TokString $ concat str

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

-- Phase 2
-- mPortalTwoWay :: Parser Token
-- mPortalEntrance :: Parser Token
-- mPortalExit :: Parser Token
-- mPortalPocketDimensionStart :: Parser Token
-- mPortalPocketDimensionEnd :: Parser Token
-- mDuplicate :: Parser Token
-- mMoveTape :: Parser Token

mLang :: Parser SourceCode
mLang = do
  whiteSpaces
  let rawParsers = [
                    mPush, mPop, mSpike, mOrb, mPortalPocketDimensionEntrance, mComparator,
                    mLever
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
