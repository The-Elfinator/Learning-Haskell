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

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus x Z = x
nplus x (S y) = nplus (S x) y 

nmult :: N -> N -> N
nmult Z _ = Z
nmult _ Z = Z
nmult x (S y) = nplus x $ nmult x y

nsub :: N -> N -> Maybe N
nsub x Z = Just x
nsub Z _ = Nothing
nsub (S x) (S y) = nsub x y

ncmp :: N -> N -> Ordering
ncmp x y = case diff of
  Nothing -> LT
  Just Z -> EQ
  _ -> GT
  where
    diff = nsub x y

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S $ nFromNatural (x-1)

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S x) = (+ 1) $ nToNum x

nEven :: N -> Bool
nEven Z = True
nEven (S Z) = False
nEven (S (S x)) = nEven x

nOdd :: N -> Bool
nOdd x = not $ nEven x

ndiv :: N -> N -> N
ndiv _ Z = error "Second argument must not be zero!"
ndiv Z _ = Z
ndiv x (S Z) = x
ndiv x y
  | ncmp x y == LT = Z
  | ncmp x y == EQ = S Z
  | otherwise = let Just diff = nsub x y in S $ ndiv diff y

nmod :: N -> N -> N
nmod _ Z = error "Second argument must not be zero!"
nmod x y = res
  where
    Just res = nsub x $ nmult y $ ndiv x y

