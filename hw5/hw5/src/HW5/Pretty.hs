{-# LANGUAGE MultiWayIf #-}

module HW5.Pretty
  ( prettyValue
  , prettyError
  ) where

import           Data.Char                     (toLower)
import           Data.Foldable
import           Data.Ratio                    (denominator, numerator)
import           Data.Scientific
import           HW5.Base
import           Prettyprinter
import           Prettyprinter.Render.Terminal


clr :: Color -> Doc AnsiStyle -> Doc AnsiStyle
clr = annotate . colorDull

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber number) =
    if denominator number == 1
        then clr Blue (pretty (numerator number))
        else case fromRationalRepetendUnlimited number of
            (val, Nothing) -> clr Blue (pretty (formatScientific Fixed Nothing val))
            _              -> clr Blue (pretty (prettyIrrational number))
prettyValue (HiValueBool x) = clr Yellow $ pretty $ map toLower $ show x
prettyValue (HiValueFunction f) = clr Magenta $ prettyFun f
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString str) = clr Green $ pretty '"' <> pretty str <> pretty '"'
prettyValue (HiValueList l) = clr Magenta $ encloseSep (pretty "[") (pretty "]") (pretty ", ") (fmap prettyValue (Data.Foldable.toList l))


prettyIrrational :: Rational -> String
prettyIrrational val
    | intPart `elem` [-1, 0] = show numeratorVal ++ "/" ++ show denominatorVal
    | intPart < 0 = show (intPart + 1) ++ " - " ++ prettyFraction denominatorVal (quotRem (-numeratorVal) denominatorVal)
    | otherwise = show intPart ++ " + " ++ prettyFraction denominatorVal (quotRem numeratorVal denominatorVal)
  where
    intPart = div numeratorVal denominatorVal
    numeratorVal = numerator val
    denominatorVal = denominator val


prettyFraction :: Integer -> (Integer, Integer) -> String
prettyFraction num (_, denom) = show denom ++ "/" ++ show num

prettyError :: HiError -> Doc AnsiStyle
prettyError e = clr Red $ viaShow e

prettyFun :: HiFun -> Doc AnsiStyle
prettyFun f = pretty (case f of
  HiFunDiv            -> "div"
  HiFunMul            -> "mul"
  HiFunSub            -> "sub"
  HiFunAdd            -> "add"
  HiFunNot            -> "not"
  HiFunAnd            -> "and"
  HiFunOr             -> "or"
  HiFunLessThan       -> "less-than"
  HiFunGreaterThan    -> "greater-than"
  HiFunEquals         -> "equals"
  HiFunNotLessThan    -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals      -> "not-equals"
  HiFunIf             -> "if"
  HiFunLength         -> "length"
  HiFunToUpper        -> "to-upper"
  HiFunToLower        -> "to-lower"
  HiFunReverse        -> "reverse"
  HiFunTrim           -> "trim"
  HiFunList           -> "list"
  HiFunFold           -> "fold"
  HiFunRange          -> "range"
  )
