module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1
    ( Fun(..), List(..), Except(..), Annotated(..), Option(..) )

joinOption :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some None) = None
joinOption (Some (Some a)) = Some a

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success a) = a


joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# b) :# c) = a :# (c <> b)

joinList :: List (List a) -> List a
joinList Nil              = Nil
joinList (a' :. b') = helper a'
  where
    helper (a'' :. b'') = a'' :. helper b''
    helper Nil                = joinList b'

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) =  F (\i -> let (F f2) = f i in f2 i)
