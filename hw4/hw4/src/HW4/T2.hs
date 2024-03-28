{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import           Control.Applicative
import           Control.Monad       (MonadPlus, mfilter, msum, void)
import           Data.Char           (digitToInt, isSpace)
import           Data.Maybe          (fromMaybe)
import           GHC.Unicode         (isDigit)
import           Numeric.Natural     (Natural)


import           HW4.T1              (ExceptState (..))
import           HW4.Types

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P parser) line = case runES parser (0, line) of
  Success (a :# _) -> Success a
  Error e          -> Error e

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))


pCharThenSkip :: Char -> Parser()
pCharThenSkip a = do
    void $ mfilter ( == a) pChar

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P p) (P q) = P $ ES $ \(pos, s) -> case runES p (pos, s) of
      Success value -> Success value
      Error _       -> runES q (pos, s)

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) -> case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)

digitToRational :: Char -> Rational
digitToRational = toRational . toInteger . digitToInt

listToIntegerPart :: [Char] -> Rational
listToIntegerPart = foldl (\acc d -> acc * 10 + digitToRational d) 0

listToDecimalPart :: [Char] -> Rational
listToDecimalPart = foldr (\d acc -> (acc + digitToRational d) / 10) 0

takeDigit :: Parser Char
takeDigit = mfilter isDigit pChar

takeInt :: Parser String
takeInt = some takeDigit

decimalPart :: Parser Rational
decimalPart = pCharThenSkip '.' *>
    takeInt >>= \number ->
    let num = listToDecimalPart number
    in return num

integralPart :: Parser Rational
integralPart = listToIntegerPart <$> takeInt

double :: Parser Double
double = fmap (\(main, sub) -> fromRational (main + fromMaybe 0 sub)) $ liftA2 (,) integralPart (optional decimalPart)

skipSpaces :: Parser ()
skipSpaces = void $ many $ mfilter isSpace pChar

type OperationParser = Parser (Expr -> Expr -> Expr)

parserOperator :: (Expr -> Expr -> Prim Expr) -> Char -> OperationParser
parserOperator operator char = (\a b -> Op $ operator a b) <$ pCharThenSkip char

mult :: OperationParser
mult = parserOperator Mul '*'

subst :: OperationParser
subst = parserOperator Sub '-'

add :: OperationParser
add = parserOperator Add '+'

division :: OperationParser
division = parserOperator Div '/'

operations :: [OperationParser] -> Parser Expr -> Parser Expr
operations p additionalParser =
    let opParser = msum p
    in leftTermOp opParser additionalParser =<< additionalParser

leftTermOp :: OperationParser -> Parser Expr -> Expr -> Parser Expr
leftTermOp ops lowLevel leftTerm =
    skipSpaces *>
    optional ops >>= \t ->
        case t of
            Just op -> lowLevel >>= \rightTerm ->
                            let expr = op leftTerm rightTerm
                            in leftTermOp ops lowLevel expr
            Nothing -> return leftTerm

lowPriority :: Parser Expr
lowPriority = operations [add, subst] highPriority

highPriority :: Parser Expr
highPriority = operations [mult, division] term

val :: Parser Expr
val = skipSpaces *> fmap Val double

term :: Parser Expr
term = val <|>
    (skipSpaces *> pCharThenSkip '(' *> lowPriority <* skipSpaces <* pCharThenSkip ')')

parseExpr :: String -> Except ParseError Expr
parseExpr = runP expression

expression :: Parser Expr
expression = lowPriority >>= \expr ->
              skipSpaces >>
              pEof >>
              return expr
