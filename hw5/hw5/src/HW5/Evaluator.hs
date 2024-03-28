{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module HW5.Evaluator
  ( eval
  ) where

import           Control.Applicative  (liftA2)
import           Control.Monad        ()
import           Control.Monad.Except
import           Data.Ratio           (denominator, numerator)
import           Data.Semigroup       (stimes)
import qualified Data.Sequence        as S
import qualified Data.Text            as T
import           HW5.Base

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExpression

type HiExcept m a = ExceptT HiError m a

evalExpression :: Monad m => HiExpr -> HiExcept m HiValue
evalExpression (HiExprValue val) = pure val
evalExpression (HiExprApply f x) = do
  fv <- evalExpression f
  case fv of
    HiValueFunction HiFunIf  -> ifBool x
    HiValueFunction HiFunAnd -> andBool x
    HiValueFunction HiFunOr  -> orBool x
    _                        -> applyFunOrError fv x

applyFunOrError :: Monad m => HiValue -> [HiExpr] -> HiExcept m HiValue
applyFunOrError fv x = do
  val <- mapM evalExpression x
  case fv of
    HiValueFunction func -> appFun func val
    HiValueString str    -> appStrIndex str val
    HiValueList list     -> appListIndex list val
    _                    -> throwError HiErrorInvalidFunction

ifBool :: Monad m => [HiExpr] -> HiExcept m HiValue
ifBool [x', y', z'] = do
  x <- evalExpression x'
  case x of
    HiValueBool True  -> evalExpression y'
    HiValueBool False -> evalExpression z'
    _                 -> throwInvalidArg
ifBool _ = throwError HiErrorArityMismatch

andBool :: Monad m => [HiExpr] -> HiExcept m HiValue
andBool [x', y'] = do
  x <- evalExpression x'
  if | isNullOrFalse x -> return x
     | otherwise       -> evalExpression y'
andBool _ = throwError HiErrorArityMismatch

orBool :: Monad m => [HiExpr] -> HiExcept m HiValue
orBool [x', y'] = do
  x <- evalExpression x'
  if | isNullOrFalse x -> evalExpression y'
     | otherwise       -> return x
orBool _ = throwError HiErrorArityMismatch


isNullOrFalse :: HiValue -> Bool
isNullOrFalse HiValueNull         = True
isNullOrFalse (HiValueBool False) = True
isNullOrFalse _                   = False

appFun :: Monad m => HiFun -> [HiValue] -> HiExcept m HiValue
appFun HiFunAdd            = binary add
appFun HiFunSub            = binary sub
appFun HiFunMul            = binary multiply
appFun HiFunDiv            = binary divide
appFun HiFunNot            = unary notBool
appFun HiFunLessThan       = binaryCmpHelper (<)
appFun HiFunGreaterThan    = binaryCmpHelper (>)
appFun HiFunEquals         = binaryCmpHelper (==)
appFun HiFunNotLessThan    = binaryCmpHelper (>=)
appFun HiFunNotGreaterThan = binaryCmpHelper (<=)
appFun HiFunNotEquals      = binaryCmpHelper (/=)
appFun HiFunLength         = unary $ lengthOf
appFun HiFunToUpper        = unaryModifyStr T.toUpper
appFun HiFunToLower        = unaryModifyStr T.toLower
appFun HiFunReverse        = unary $ reverseString
appFun HiFunTrim           = unaryModifyStr T.strip
appFun HiFunList           = appList
appFun HiFunFold           = binary appFold
appFun HiFunRange          = appRange
appFun _                   = const throwInvalidArg

-- List functions
appList, appRange :: Monad m => [HiValue] -> HiExcept m HiValue
appList = return . HiValueList . S.fromList

appRange [HiValueNumber x, HiValueNumber y] = return $ HiValueList $ fmap HiValueNumber $ S.fromList [x..y]
appRange _ = throwInvalidArg

-- Binary functions
appFold :: Monad m => HiValue -> HiValue -> HiExcept m HiValue
appFold (HiValueFunction func) (HiValueList (headValue S.:<| tailValues)) =
  foldM (\a b -> appFun func [a, b]) headValue tailValues
appFold _ _ = throwInvalidArg

add, sub, multiply, divide :: Monad m => HiValue -> HiValue -> HiExcept m HiValue
add (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber $ x + y
add (HiValueString x) (HiValueString y) = return $ HiValueString $ x <> y
add (HiValueList x) (HiValueList y)     = return $ HiValueList $ x <> y
add _ _                                 = throwInvalidArg

sub (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber $ x - y
sub _ _                                 = throwInvalidArg

multiply (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber $ x * y
multiply (HiValueString x) (HiValueNumber y)
  | y <= 0 = throwInvalidArg
  | otherwise = do
      mr <- rationalToInt y
      return $ HiValueString $ stimes mr x
multiply (HiValueList x) (HiValueNumber y)
  | y <= 0 = throwInvalidArg
  | otherwise = do
      mr <- rationalToInt y
      return $ HiValueList $ stimes mr x
multiply _ _ = throwInvalidArg

divide (HiValueNumber x) (HiValueNumber y)
  | y == 0    = throwError HiErrorDivideByZero
  | otherwise = return $ HiValueNumber $ x / y
divide (HiValueString x) (HiValueString y) = return $ HiValueString $ x <> T.pack "/" <> y
divide _ _     = throwInvalidArg

-- Unary function
notBool :: Monad m => HiValue -> HiExcept m HiValue
notBool (HiValueBool x) = return $ HiValueBool $ not x
notBool _               = throwInvalidArg

-- Helper for binary comparison functions
binaryCmpHelper :: Monad m => (HiValue -> HiValue -> Bool) -> [HiValue] -> HiExcept m HiValue
binaryCmpHelper f [x, y] = return $ HiValueBool $ f x y
binaryCmpHelper _ _      = throwError HiErrorArityMismatch

-- Other helper functions
unary :: Monad m => (HiValue -> HiExcept m HiValue) -> [HiValue] -> HiExcept m HiValue
unary f [x] = f x
unary _ _   = throwError HiErrorArityMismatch

unaryModifyStr :: Monad m => (T.Text -> T.Text) -> [HiValue] -> HiExcept m HiValue
unaryModifyStr f = unary $ \case
  HiValueString s -> return $ HiValueString $ f s
  _               -> throwInvalidArg

binary :: Monad m => (HiValue -> HiValue -> HiExcept m HiValue) -> [HiValue] -> HiExcept m HiValue
binary f [x, y] = f x y
binary _ _      = throwError HiErrorArityMismatch

lengthOf, reverseString :: Monad m => HiValue -> HiExcept m HiValue
lengthOf (HiValueString s) = return . HiValueNumber . toRational $ T.length s
lengthOf (HiValueList l)   = return . HiValueNumber . toRational $ S.length l
lengthOf _                 = throwInvalidArg

reverseString (HiValueString s) = return $ HiValueString $ T.reverse s
reverseString (HiValueList l)   = return $ HiValueList $ S.reverse l
reverseString _                 = throwInvalidArg

throwInvalidArg :: Monad m => HiExcept m a
throwInvalidArg = throwError HiErrorInvalidArgument

appStrIndex :: Monad m => T.Text -> [HiValue] -> HiExcept m HiValue
appStrIndex s args =
    case args of
        [HiValueNumber i'] -> do
            i <- rationalToInt i'
            return $ if i < 0 || i >= len then HiValueNull else HiValueString . T.singleton $ T.index s i
        [i', j'] -> liftA2 slice (getIndexOrDefault i' 0 len) (getIndexOrDefault j' len len)
        _ -> throwError HiErrorArityMismatch
  where
    len = T.length s
    slice i j = HiValueString $ T.drop i $ T.take j s

appListIndex :: Monad m => S.Seq HiValue -> [HiValue] -> HiExcept m HiValue
appListIndex l args =
     case args of
         [HiValueNumber i'] -> do
             i <- rationalToInt i'
             return $ if i < 0 || i >= len then HiValueNull else S.index l i
         [i', j'] -> liftA2 slice (getIndexOrDefault i' 0 len) (getIndexOrDefault j' len len)
         _ -> throwError HiErrorArityMismatch
   where
     len = S.length l
     slice i j = HiValueList $ S.drop i $ S.take j l

getIndexOrDefault :: Monad m => HiValue -> Int -> Int -> HiExcept m Int
getIndexOrDefault (HiValueNumber x') _ y' = do
    x <- rationalToInt x'
    return $ if x >= 0 then x else (x + y')
getIndexOrDefault HiValueNull def _ = return def
getIndexOrDefault _ _ _ = throwInvalidArg


rationalToInt :: Monad m => Rational -> HiExcept m Int
rationalToInt val
  | denominator val == 1 = pure $ fromInteger $ numerator val
  | otherwise = throwInvalidArg
