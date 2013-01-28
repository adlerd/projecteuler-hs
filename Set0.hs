module Set0 (set0) where

import Atkin (primes)
import Sorted (union, count, uncount)
import EulerUtil (factors, by, allBut, iSqrt, slide,fibonacci)
import Data.List (sort, groupBy, maximumBy, tails)
import Data.Ord (comparing)
import Input (input8)
import Data.Maybe (catMaybes)

set0 :: [(Int, String)]
set0 = zip [1..]
       [euler1,euler2,euler3,euler4,euler5,euler6,euler7,euler8,euler9]

euler1 = show . sum $ union [3,6..999] [5,10..999]

euler2 = show . sum . filter even . takeWhile (< 4000000) $ fibonacci

euler3 = show . last . factors $ 600851475143

euler4 = show . last . sort
         . concatMap (lastCons . filter (isPalindrome . show)) $ products
    where
      isPalindrome xs = xs == reverse xs
      products = [[x*y | y <- [x..999]] | x <- [100..999]]
      lastCons [] = []
      lastCons x = [last x]

euler5 = show . product . uncount . map (maximumBy . comparing $ snd)
         . groupBy ((==) `by` fst) . sort . concatMap (count . factors)
         $ [1..20]

euler6 = show $ ((^ 2) . sum $ [1..100]) - (sum . map (^ 2) $ [1..100])

euler7 = show $ primes !! 10000

euler8 = show . maximum . map product . filter (not . elem 0) . slide 5 $ input8

euler9 = show . product3 . head . filter ((== 1000) . sum3) . catMaybes
         $ [testPair a b | b <- [2..], a <- [1..b]]
    where
      sum3 (a,b,c) = a + b + c
      product3 (a,b,c) = a * b * c
      testPair a b = do c <- iSqrt (a^2+b^2)
                        return (a,b,c)
