module Helper.List where

import Data.Sort as Sort

--------------------------------------------------------------------------------

removeInstances :: Eq a => Int -> a -> [a] -> [a]
removeInstances _ _ [] = []
removeInstances 0 _ xs = xs

removeInstances n y (x:xs)
  | x == y    = recurse (n - 1)
  | otherwise = x : recurse n
  where recurse n = removeInstances n y xs

--------------------------------------------------------------------------------

removeSingle :: Eq a => a -> [a] -> [a]
removeSingle = removeInstances 1

--------------------------------------------------------------------------------

insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x:xs)
  | x < y     = y:x:xs
  | otherwise = x:(insert y xs)

--------------------------------------------------------------------------------

removeInstances' :: Int -> (a -> Bool) -> [a] -> [a]
removeInstances' _ _ [] = []
removeInstances' 0 _ xs = xs

removeInstances' n eq (x:xs)
  | eq x      = recurse (n - 1)
  | otherwise = x : recurse n
  where recurse n = removeInstances' n eq xs

--------------------------------------------------------------------------------

removeSingle' :: (a -> Bool) -> [a] -> [a]
removeSingle' = removeInstances' 1

--------------------------------------------------------------------------------
