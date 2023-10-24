module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int, Int, Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (leftSize, rightSize, _, _) left value right) = 
  leftSize + 1 + rightSize

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, _, leftDepth, rightDepth) left value right) = 
  (+ 1) $ max leftDepth rightDepth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember value (Branch _ left nodeValue right)
  | value == nodeValue = True
  | value < nodeValue = tmember value left
  | otherwise = tmember value right

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert value Leaf = Branch (0, 0, 0, 0) Leaf value Leaf
tinsert value (Branch (0, 0, 0, 0) Leaf nodeValue Leaf)
  | value < nodeValue = Branch (1, 0, 1, 0) newLeaf nodeValue Leaf
  | otherwise = Branch (0, 1, 0, 1) Leaf nodeValue newLeaf
  where
    newLeaf = tinsert value Leaf
tinsert value (Branch (lSize, rSize, lDepth, rDepth) left nodeValue right)
  | value < nodeValue = Branch (newLSize, rSize, newLDepth, rDepth) newLeft nodeValue right
  | otherwise = Branch (lSize, newRSize, lDepth, newRDepth) left nodeValue newRight
  where
    newLeft = tinsert value left
    newRight = tinsert value right
    newLSize = tsize newLeft
    newRSize = tsize newRight
    newLDepth = tdepth newLeft
    newRDepth = tdepth newRight

tFromList :: Ord a => [a] -> Tree a
tFromList [] = Leaf
tFromList (x:xs) = helper Leaf (x:xs)
  where
    helper :: Ord a => Tree a -> [a] -> Tree a
    helper tree [] = tree
    helper tree (x:xs) = helper (tinsert x tree) xs

