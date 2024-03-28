{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module HW5.Base
  ( HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiValue(..)
  ) where

import           Data.Sequence (Seq)
import           Data.Text     (Text)


data HiFun =
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold deriving (Eq, Show)



data HiValue =
  HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue) deriving (Eq, Show)


data HiExpr =
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr] deriving (Eq, Show)


data HiError =
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero deriving (Eq, Show)


instance Ord HiValue where
  (<=) (HiValueNumber x) (HiValueNumber y) = x <= y
  (<=) (HiValueBool x) (HiValueBool y)     = x <= y
  (<=) HiValueNumber{} HiValueBool{}       = False
  (<=) HiValueBool{} HiValueNumber{}       = True
  (<=) _ _                                 = True
