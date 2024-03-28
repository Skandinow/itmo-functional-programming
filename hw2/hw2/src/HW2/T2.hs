module HW2.T2
  ( joinWith
  , splitOn
  ) where

import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty (..), toList, (<|))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn del = foldr f ([] :| [])
  where
    f el (x :| xs) = case el == del of
      True  -> [] <| (x :| xs)
      False -> (el : x) :| xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith x = foldl1 (join x)
  where
   join x line = (++) (line ++ [x])
