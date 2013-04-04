module Set9 (set9) where

import EulerUtil (rCombinations, digits)
import Data.List (sort)

set9 :: [(Int, String)]
set9 = zip [90..]
       [euler90,euler91]

euler90 = show . length . filter valid . pairs $ cubes
  where
    cubes = rCombinations 6 [0..9]
    pairs [] = []
    pairs (x:xs) = map ((,) x) xs ++ pairs xs
    valid (a,b) = all (makes . digits . (^2)) [1..9]
      where
        makes [x] = makes [0,x]
        makes [x,y] = x `elem'` a && y `elem'` b || x `elem'` b && y `elem'` a
    elem' 6 l = elem' 9 l
    elem' 9 l = elem 9 l || elem 6 l
    elem' x l = elem x l

euler91 = show . length . filter valid . pairs $ points
  where
    pairs [] = []
    pairs (x:xs) = map ((,) x) xs ++ pairs xs
    points = tail [ (x,y) | x <- [0..50], y <- [0..50] ]
    valid (p1@(x1,y1),p2@(x2,y2)) = side1+side2 == side3
      where
        [side1,side2,side3] = sort [len p1,len p2,len (x1-x2,y1-y2)]
        len (x,y) = x*x+y*y
