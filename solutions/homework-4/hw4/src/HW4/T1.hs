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

import HW4.Types
import Control.Monad (ap)

newtype ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES runExcept) = ES $ \x -> getResult f $ runExcept x
  where
    getResult :: (a0 -> b0) -> Except e0 (Annotated s0 a0) -> Except e0 (Annotated s0 b0)
    getResult _ (Error e)               = Error e
    getResult func (Success annotation) = Success (mapAnnotated func annotation)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState value = ES $ \x -> Success (value :# x)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES runExcept) = ES $ \x -> createJoinedState $ runExcept x
  where
    createJoinedState :: Except e0 (Annotated s0 (ExceptState e0 s0 a0)) -> 
                         Except e0 (Annotated s0 a0)
    createJoinedState (Error e)                            = Error e
    createJoinedState (Success (ES runESInner :# infoOut)) = runESInner infoOut

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \x -> Success (() :# f x)

throwExceptState :: e -> ExceptState e s a
throwExceptState err = ES $ \_ -> Error err

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  (>>=) state f = joinExceptState $ fmap f state

data EvaluationError = DivideByZero
  deriving Show

evalUnOp :: (Double -> Prim Double) -> Expr -> 
            (Double -> Double) -> 
            ExceptState EvaluationError [Prim Double] Double
evalUnOp operName arg unarOper = do
  res <- eval arg
  modifyExceptState (operName res :)
  return $ unarOper res

evalBinOp :: (Double -> Double -> Prim Double) -> Expr -> Expr ->
             (Double -> Double -> Double) -> 
             ExceptState EvaluationError [Prim Double] Double
evalBinOp operName arg1 arg2 binOper = do
  res1 <- eval arg1
  res2 <- eval arg2
  modifyExceptState (operName res1 res2 :)
  return $ binOper res1 res2

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = return x
eval (Op (Abs op)) = evalUnOp Abs op abs
eval (Op (Sgn op)) = evalUnOp Sgn op signum
eval (Op (Add op1 op2)) = evalBinOp Add op1 op2 (+)
eval (Op (Sub op1 op2)) = evalBinOp Sub op1 op2 (-)
eval (Op (Mul op1 op2)) = evalBinOp Mul op1 op2 (*)
eval (Op (Div op1 op2)) = do
  res1 <- eval op1
  res2 <- eval op2
  if (res2 == 0) 
    then throwExceptState DivideByZero 
    else evalBinOp Div (Val res1) (Val res2) (/)

