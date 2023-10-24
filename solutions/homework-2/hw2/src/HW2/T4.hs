module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  Last x <> y = x :+ y
  (headX :+ tailX) <> Last y = headX :+ (tailX <> Last y)
  x <> (headY :+ tailY) = (x <> Last headY) <> tailY

data Inclusive a b = This a | That b | Both a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This x <> This y = This (x <> y)
  This x <> That y = Both x y
  This x <> Both y1 y2 = Both (x <> y1) y2
  That x <> This y = Both y x
  That x <> That y = That (x <> y)
  That x <> Both y1 y2 = Both y1 (x <> y2)
  Both x1 x2 <> This y = Both (x1 <> y) x2
  Both x1 x2 <> That y = Both x1 (x2 <> y)
  Both x1 x2 <> Both y1 y2 = Both (x1 <> y1) (x2 <> y2)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  DS x <> DS "" = DS x
  DS "" <> DS x = DS x
  DS x <> DS y = DS (x ++ "." ++ y)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f1 <> F f2 = F (f1 . f2)

instance Monoid (Fun a) where
  mempty = F id

