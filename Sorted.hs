module Sorted where

merge,union :: (Ord a) => [a] -> [a] -> [a]
mergeMany,mergeManyStable :: (Ord a) => [[a]] -> [a]
nub :: (Ord a) => [a] -> [a]

merge [] yy = yy
merge xx [] = xx
merge xx@(x:xs) yy@(y:ys)
    | y < x = y : merge xx ys
    | otherwise = x : merge xs yy

mergeMany [] = []
mergeMany [x] = x
mergeMany xs = head $ until single mm xs
    where
      single [_] = True
      single _ = False
      mm :: (Ord a) => [[a]] -> [[a]]
      mm [] = []
      mm [x,y,z] = [z, merge x y]
      mm (x:y:zs) = merge x y : mm zs

mergeManyStable [] = []
mergeManyStable [x] = x
mergeManyStable xs = head $ until single mm xs
    where
      single [_] = True
      single _ = False
      mm :: (Ord a) => [[a]] -> [[a]]
      mm [] = []
      mm [z] = [z]
      mm (x:y:zs) = merge x y : mm zs

union [] yy = yy
union xx [] = xx
union xx@(x:xs) yy@(y:ys)
    | y < x = y : union xx ys
    | x < y = x : union xs yy
    | otherwise = union xx ys

nub [] = []
nub [x] = [x]
nub (x:xs@(y:ys))
    | x == y = nub (x:ys)
    | otherwise = x : nub xs

count [] = []
count (x:xs) = count' x 1 xs
    where
      count' item ct [] = [(item, ct)]
      count' item ct (x:xs)
          | x == item = count' item (ct + 1) xs
          | otherwise = (item, ct) : count' x 1 xs

uncount [] = []
uncount ((i,c):rest)
    | c == 1 = i : uncount rest
    | c > 1 = i : uncount ((i,c-1):rest)
    | c == 0 = uncount rest
