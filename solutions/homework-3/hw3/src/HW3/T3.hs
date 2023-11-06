module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1
import HW3.T2 (concatLists)

joinOption :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some x) = x

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error x) = Error x
joinExcept (Success y) = y

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# y1) :# y2) = x :# y2 <> y1

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (lst :. other) = concatLists lst $ joinList other

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\x -> f x `g` x)
  where
    g :: Fun i a -> i -> a
    g (F innerFunc) arg = innerFunc arg

