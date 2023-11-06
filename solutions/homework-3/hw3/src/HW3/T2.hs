module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  , concatLists
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (_, None) = None
distOption (None, _) = None
distOption (Some x, Some y) = Some (x, y)

wrapOption :: a -> Option a
wrapOption x = Some x

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x1 x2, P y1 y2) = P (x1, y1) (x2, y2)

wrapPair :: a -> Pair a
wrapPair x = P x x

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x1 x2 x3 x4, Q y1 y2 y3 y4) = Q (x1, y1) (x2, y2) (x3, y3) (x4, y4)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x1 :# y1, x2 :# y2) = (x1, x2) :# y1 <> y2

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error x, _) = Error x
distExcept (_, Error y) = Error y
distExcept (Success x, Success y) = Success (x, y)

wrapExcept :: a -> Except e a
wrapExcept x = Success x

fromPrior :: Prioritised a -> Integer
fromPrior (Low _) = 0
fromPrior (Medium _) = 1
fromPrior (High _) = 2

getMax :: Integer -> Integer -> Integer
getMax x y | x == 0 = y
           | x == 1 && y == 0 = 1
           | x == 1 = y
           | otherwise = 2

getHigherPrior :: Prioritised a -> Prioritised b -> Integer
getHigherPrior x y = fromPrior x `getMax` fromPrior y

getValue :: Prioritised a -> a
getValue (Low x) = x
getValue (Medium x) = x
getValue (High x) = x

makePair :: Prioritised a -> Prioritised b -> (a, b)
makePair x y = (getValue x, getValue y)

toPrior :: Integer -> a -> Prioritised a
toPrior prior value | prior == 0 = Low value
                    | prior == 1 = Medium value
                    | otherwise = High value

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (x, y) = getHigherPrior x y `toPrior` makePair x y

wrapPrioritised :: a -> Prioritised a
wrapPrioritised x = Low x

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x1 :> y1, x2 :> y2) = (x1, x2) :> distStream (y1, y2)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

concatLists :: List a -> List a -> List a
concatLists Nil y = y
concatLists x Nil = x
concatLists (x1 :. Nil) y = x1 :. y
concatLists (x1 :. x2) y = x1 :. concatLists x2 y

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (x :. Nil, y1 :. y2) = (x, y1) :. distList (x :. Nil, y2) 
distList (x1 :. x2, y) = distList (x1 :. Nil, y) `concatLists` distList (x2, y)

wrapList :: a -> List a
wrapList x = x :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\x -> (f x, g x))

wrapFun :: a -> Fun i a
wrapFun x = F (\_ -> x)

