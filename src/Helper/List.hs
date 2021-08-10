module Helper.List where

import Data.Sort as Sort

--------------------------------------------------------------------------------

removeInstances :: Eq a => Int -> a -> [a] -> Maybe [a]
removeInstances 0 _ xs = return xs
removeInstances _ _ [] = Nothing

removeInstances n y (x:xs)
  | x == y    = recurse (n - 1)
  | otherwise = recurse n >>= return . (:) x
  where recurse n = removeInstances n y xs

--------------------------------------------------------------------------------

removeSingle :: Eq a => a -> [a] -> Maybe [a]
removeSingle = removeInstances 1

--------------------------------------------------------------------------------

insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x:xs)
  | x < y     = y:x:xs
  | otherwise = x:(insert y xs)

--------------------------------------------------------------------------------

removeInstances' :: Int -> (a -> Bool) -> [a] -> Maybe [a]
removeInstances' 0 _ xs = return xs
removeInstances' _ _ [] = Nothing

removeInstances' n cond (x:xs)
  | cond x    = recurse (n - 1)
  | otherwise = recurse n >>= return . (:) x
  where recurse n = removeInstances' n cond xs

--------------------------------------------------------------------------------

removeSingle' :: (a -> Bool) -> [a] -> Maybe [a]
removeSingle' = removeInstances' 1

--------------------------------------------------------------------------------
