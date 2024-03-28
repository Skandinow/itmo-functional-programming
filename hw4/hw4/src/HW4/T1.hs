module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import qualified Control.Monad
import           HW4.Types


data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (x :# e) = f x :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success $ f a

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES exceptor) = ES (\s ->
    let innerComp = exceptor s
    in mapExcept (mapAnnotated f) innerComp)


wrapExcept :: a -> Except e a
wrapExcept = Success

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\s -> wrapExcept (a :# s))

extractExcept :: Except e (Annotated s (ExceptState e s a)) -> Except e (Annotated s a)
extractExcept (Error error')                  = Error error'
extractExcept (Success (state :# annotation)) = runES state annotation

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES exceptor) = ES (\s -> extractExcept (exceptor s))

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (():#f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) p q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  (>>=) m f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val val) = wrapExceptState val
eval (Op (Add left right)) = evalBin left right (\x y -> (x + y, Add x y))
eval (Op (Sub left right)) = evalBin left right (\x y -> (x - y, Sub x y))
eval (Op (Mul left right)) = evalBin left right (\x y -> (x * y, Mul x y))
eval (Op (Abs val)) = evalUnary val (\x -> (abs x, Abs x))
eval (Op (Sgn val)) = evalUnary val (\x -> (signum x, Sgn x))
eval (Op (Div x1 x2)) = do
  res1 <- eval x1
  res2 <- eval x2
  modifyExceptState ([Div res1 res2] ++)
  if res2 == 0
    then throwExceptState DivideByZero
  else
    return $ res1 / res2

evalBin :: Expr
           -> Expr
           -> (Double -> Double -> (Double, Prim Double))
           -> ExceptState EvaluationError [Prim Double] Double
evalBin left right f = do
  valueLeft <- eval left
  valueRight <- eval right
  let (result, primResult) = f valueLeft valueRight
  modifyExceptState (primResult :)
  pure result

evalUnary :: Expr
          -> (Double -> (Double, Prim Double))
          -> ExceptState EvaluationError [Prim Double] Double
evalUnary val f = do
  value <- eval val
  let (result, primResult) = f value
  modifyExceptState (primResult :)
  pure result
