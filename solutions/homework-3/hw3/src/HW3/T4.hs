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

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S runState) = S $ \x -> mapAnnotated f $ runState x 

wrapState :: a -> State s a
wrapState value = S $ \x -> value :# x

joinState :: State s (State s a) -> State s a
joinState (S runState) = S $ \x -> createAnnot (runState x)
  where
    createAnnot :: Annotated s0 (State s0 a0) -> Annotated s0 a0
    createAnnot (S runSInner :# annotOut) = runSInner annotOut

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \x -> () :# f x

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) (S runSFunc) stateArg = S $ \x -> runSFunc x `createAnnot` stateArg
    where
      createAnnot :: Annotated s0 (a0 -> b0) -> State s0 a0 -> Annotated s0 b0
      createAnnot (func :# annotFunc) (S runSArg) = func `mapAnnotated` runSArg annotFunc

instance Monad (State s) where
  (>>=) (S runSArg) f = S $ \x -> runSArg x `createAnnot` f
    where
      createAnnot :: Annotated s0 a0 -> (a0 -> State s0 b0) -> Annotated s0 b0
      createAnnot (argValue :# argAnnot) func = func argValue `unpackAnnot` argAnnot

      unpackAnnot :: State s1 b1 -> s1 -> Annotated s1 b1
      unpackAnnot (S runSRes) argToRun = runSRes argToRun

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
  (/) x y = Op (Div x y)
  fromRational x = Val (fromRational x)

evalUnOp :: (Double -> Prim Double) -> Expr -> 
  (Double -> Double) -> State [Prim Double] Double
evalUnOp operName arg unarOper = do
  res <- eval arg
  modifyState (operName res :)
  return $ unarOper res

evalBinOp :: (Double -> Double -> Prim Double) -> Expr -> Expr ->
  (Double -> Double -> Double) -> State [Prim Double] Double
evalBinOp operName arg1 arg2 binOper = do
  res1 <- eval arg1
  res2 <- eval arg2
  modifyState (operName res1 res2 :)
  return $ binOper res1 res2

eval :: Expr -> State [Prim Double] Double
eval (Val x) = return x
eval (Op (Abs op)) = evalUnOp Abs op abs
eval (Op (Sgn op)) = evalUnOp Sgn op signum
eval (Op (Add op1 op2)) = evalBinOp Add op1 op2 (+)
eval (Op (Sub op1 op2)) = evalBinOp Sub op1 op2 (-)
eval (Op (Mul op1 op2)) = evalBinOp Mul op1 op2 (*)
eval (Op (Div op1 op2)) = evalBinOp Div op1 op2 (/)

