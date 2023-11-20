{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad
import Data.Char (isDigit, digitToInt, isSpace)
import GHC.Float (int2Double)

import HW4.Types
import HW4.T1 (ExceptState(..))

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P (ES runState)) input = getExcept $ runState (0, input)
  where
    getExcept :: Except ParseError (Annotated (Natural, String) a0) -> Except ParseError a0
    getExcept (Error err)          = Error err
    getExcept (Success (res :# _)) = Success res

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error $ ErrorAtPos pos

instance Alternative Parser where
  empty = parseError
  (<|>) (P (ES runState1)) (P (ES runState2)) = P $ ES $ \(pos, input) -> 
    runState1 (pos, input) `getResult` runState2 (pos, input)
      where
        getResult :: Except ParseError (Annotated (Natural, String) a) -> 
                     Except ParseError (Annotated (Natural, String) a) -> 
                     Except ParseError (Annotated (Natural, String) a)
        getResult (Success x) _ = Success x
        getResult (Error _) y   = y

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) -> 
  case s of
    [] -> Success (() :# (pos, ""))
    _  -> Error $ ErrorAtPos pos

pPredicat :: (Char -> Bool) -> Parser Char
pPredicat cond = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error $ ErrorAtPos pos
    (c:cs) -> if cond c then Success (c :# (pos + 1, cs)) else Error $ ErrorAtPos pos

parseChar :: Char -> Parser Char
parseChar c = pPredicat (== c)

pIsSpace :: Parser Char
pIsSpace = pPredicat isSpace

skipSpaces :: Parser String
skipSpaces = many pIsSpace

pIsDigit :: Parser Char
pIsDigit = pPredicat isDigit

parseDigit :: Parser Double
parseDigit = int2Double . digitToInt <$> pIsDigit

makeDouble :: (Double -> [Double] -> Double) -> Parser Double
makeDouble func = func 0.0 <$> some parseDigit

parseIntPart :: Parser Double
parseIntPart = makeDouble $ foldl (\x y -> x * 10 + y)

parseDelimiter :: Parser Char
parseDelimiter = parseChar '.'

parseFracPart :: Parser Double
parseFracPart = makeDouble $ foldr (\x y -> (x + y) / 10)

parseDouble :: Parser Double
parseDouble = do
  intPart <- parseIntPart
  opt <- optional $ do
    void parseDelimiter
    parseFracPart
  return $ case opt of
    Nothing       -> intPart
    Just fracPart -> intPart + fracPart

parseConst :: Parser Expr
parseConst = do
  sgn <- optional parseMinus
  x <- parseDouble
  return $ Val $ case sgn of
    Nothing -> x
    Just _  -> (-x)

data Smb = Plus | Minus | Star | Slash

parsePlus :: Parser Smb
parsePlus = do 
  void $ parseChar '+'
  return Plus

parseMinus :: Parser Smb
parseMinus = do
  void $ parseChar '-'
  return Minus

parseStar :: Parser Smb
parseStar = do
  void $ parseChar '*'
  return Star

parseSlash :: Parser Smb
parseSlash = do
  void $ parseChar '/'
  return Slash

parseLeftBr :: Parser Char
parseLeftBr = parseChar '('

parseRightBr :: Parser Char
parseRightBr = parseChar ')'

pEndExpr :: Expr -> Parser Expr
pEndExpr arg = do
  pEof <|> void parseRightBr
  return arg

parseBrackets :: Parser Expr
parseBrackets = do
  void parseLeftBr
  parseExpression

parseConstOrExpr :: Parser Expr
parseConstOrExpr = do
  void skipSpaces
  y <- parseBrackets <|> parseConst
  void skipSpaces
  return y

parseLowerPrior :: Expr -> Parser Expr
parseLowerPrior arg = pEndExpr arg <|> parseAddSub arg

createOperation :: Smb -> (Expr -> Expr -> Prim Expr)
createOperation Plus = Add
createOperation Minus = Sub
createOperation Star = Mul
createOperation Slash = Div

parseAddSub :: Expr -> Parser Expr
parseAddSub x = do
  op <- parsePlus <|> parseMinus
  y <- parseHigherPrior
  parseLowerPrior $ Op $ (createOperation op) x y 

parseMulDiv :: Expr -> Parser Expr
parseMulDiv x = do
  op <- parseStar <|> parseSlash
  y <- parseConstOrExpr
  parseMidPrior $ Op $ (createOperation op) x y

parseMidPrior :: Expr -> Parser Expr
parseMidPrior arg = parseMulDiv arg <|> return arg

parseHigherPrior :: Parser Expr
parseHigherPrior = do
  x <- parseConstOrExpr
  parseMidPrior x

parseExpression :: Parser Expr
parseExpression = do
  x <- parseHigherPrior
  parseLowerPrior x

-- E -> TE'  ::   E is expression, T is parsePrior, E' is parseAddSub
-- E' -> eps | +TE' | -TE' :: eps is Eof
-- T -> FT' :: F is parseConst, T' is parseMulDiv
-- T' -> eps | *FT' | /FT'
-- F -> n | (E)
-- 1 + 1 
-- E -> TE' -> T+TE' -> FT' + FT' -> n + n
parseExpr :: String -> Except ParseError Expr
parseExpr = runP parseExpression

