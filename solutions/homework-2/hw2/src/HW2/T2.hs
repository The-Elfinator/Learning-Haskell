module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty ( NonEmpty(..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = [] :| []
splitOn sep (current:other) | sep == current = [] :| headRes : tailRes
                            | otherwise = (current : headRes) :| tailRes
  where
    headRes :| tailRes = splitOn sep other

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ ([] :| []) = []
joinWith sep ([] :| nextList : tailSource) = sep : joinWith sep (nextList :| tailSource)
joinWith sep ((current : other) :| tailSource) = current : joinWith sep (other :| tailSource)

