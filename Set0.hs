module Set0 (set0) where

import Atkin (primes)
import Sorted (union, count, uncount)
import EulerUtil (factors, allBut, iSqrt, slide, fibonacci, isPalindrome, justFind)
import Data.List (sort, groupBy, maximumBy, tails, foldl1')
import Data.Ord (comparing)
import Input (input8)
import Data.Maybe (catMaybes)
import Control.Arrow ((&&&))
import PTriplets (pythags)

set0 :: [(Int, String)]
set0 = zip [1..]
       [euler1,euler2,euler3,euler4,euler5,euler6,euler7,euler8,euler9]

euler1 = show . sum $ union [3,6..999] [5,10..999]

euler2 = show . sum . filter even . takeWhile (< 4000000) $ fibonacci

euler3 = show . last . factors $ 600851475143

euler4 = show . maximum . catMaybes
         . map (last' . filter (isPalindrome . show)) $ products
    where
      products = [[x*y | y <- [x..999]] | x <- [100..999]]
      last' [] = Nothing
      last' xs = Just . last $ xs

euler5 = show . foldl1' lcm $ [1..20]

euler6 = show . uncurry (-) . (sq . sum &&& sum . map sq) $ [1..100]
    where
      sq = (^ 2)

euler7 = show $ primes !! 10000

euler8 = show . maximum . map product . slide 5 $ input8

euler9 = show . product3 . justFind ((== 1000) . sum3) $ pythags
    where
      sum3 (a,b,c) = a + b + c
      product3 (a,b,c) = a * b * c
