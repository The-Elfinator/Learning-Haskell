module HW2.T3
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap helper
  where
    helper :: Monoid a1 => Maybe a1 -> a1
    helper (Just element) = element
    helper Nothing = mempty
 
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap helper
  where
    helper :: (Monoid a1, Monoid b1) => Either a1 b1 -> (a1, b1)
    helper (Left x) = (x, mempty)
    helper (Right y) = (mempty, y)

