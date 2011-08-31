module EulerUtil where

import Atkin
import Data.List (unfoldr)

factors x = factors' primes x
    where
      factors' _ 1 = []
      factors' pp@(p:ps) x
          | m == 0 = p : factors' pp d
          | otherwise = factors' ps x
          where
            (d,m) = x `quotRem` p

by :: (a -> a -> b) -> (c -> a) -> (c -> c -> b)
by d m x y = (m x) `d` (m y)

allBut :: Int -> [a] -> [a]
allBut n xs = zipWith (\a b -> a) xs $ drop n xs
