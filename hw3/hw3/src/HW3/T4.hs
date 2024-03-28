module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1
import qualified Control.Monad

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState fun state = S (mapAnnotated fun . runS state)


wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState state = S (\s -> let innerState :# annotation = runS state s
                            in runS innerState annotation)


modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) p q = Control.Monad.ap p q

instance Monad (State s) where
  (>>=) m f = joinState (fmap f m)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) x y = Op (Add x y)
  (-) x y = Op (Sub x y)
  (*) x y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
   x / y = Op (Div x y)
   fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval (Val val) = wrapState val
eval (Op (Add left right)) = evalBin left right (\x y -> (x + y, Add x y))
eval (Op (Sub left right)) = evalBin left right (\x y -> (x - y, Sub x y))
eval (Op (Mul left right)) = evalBin left right (\x y -> (x * y, Mul x y))
eval (Op (Div left right)) = evalBin left right (\x y -> (x / y, Div x y))
eval (Op (Abs val)) = evalUnary val (\x -> (abs x, Abs x))
eval (Op (Sgn val)) = evalUnary val (\x -> (signum x, Sgn x))

evalBin :: Expr -> Expr -> (Double -> Double -> (Double, Prim Double)) -> State [Prim Double] Double
evalBin left right f = do
  valueLeft <- eval left
  valueRight <- eval right
  let (result, primResult) = f valueLeft valueRight
  modifyState (primResult :)
  pure result

evalUnary :: Expr -> (Double -> (Double, Prim Double)) -> State [Prim Double] Double
evalUnary val f = do
  value <- eval val
  let (result, primResult) = f value
  modifyState (primResult :)
  pure result