module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import           Data.Maybe      (fromJust)
import           Numeric.Natural


data N = Z | S N deriving Show

nplus :: N -> N -> N
nplus Z b     = b
nplus (S a) b = S (nplus a b)

nmult :: N -> N -> N
nmult Z _     = Z
nmult _ Z     = Z
nmult a (S b) = nplus a (nmult a b)

nsub :: N -> N -> Maybe N
nsub a Z         =  Just a
nsub (S a) (S b) = nsub a b
nsub _ _         = Nothing

ncmp :: N -> N -> Ordering
ncmp a b = case substract of
  Just Z  -> EQ
  Nothing -> LT
  _       -> GT
  where
    substract = nsub a b

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = nplus (S Z) (nFromNatural (pred x))

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = nToNum n + 1

ndiv :: N -> N -> N
ndiv _ Z = error "division by zero"
ndiv a b = case ncmp a b of
  LT -> Z
  _  -> nplus (S Z) (ndiv (fromJust (nsub a b)) b)

nEven :: N -> Bool
nEven a = case nmod a (S (S Z)) of
  Z -> True
  _ -> False

nOdd :: N -> Bool
nOdd = not . nEven

nmod :: N -> N -> N
nmod _ Z = error "division by zero"
nmod a b = fromJust (nsub a (nmult b (ndiv a b)))
