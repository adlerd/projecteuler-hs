module Set9 (set9) where

import EulerUtil (rCombinations, digits)

set9 :: [(Int, String)]
set9 = zip [90..]
       [euler90]

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
