module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ collection Leaf = collection
tfoldr func oldColl (Branch _ left nodeValue right) = tfoldr func newColl left
  where
    newColl = func nodeValue rightColl
    rightColl = tfoldr func oldColl right

