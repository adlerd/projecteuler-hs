module Set0 (set0) where

import Atkin (primes)
import Sorted (union, count, uncount)
import EulerUtil (factors, by, allBut)
import Data.List (sort, groupBy, maximumBy, tails)
import Data.Ord (comparing)
import Input (input8)

set0 = [undefined,euler1,euler2,euler3,euler4,euler5,euler6,euler7,euler8]

euler1 = show . sum $ union [3,6..999] [5,10..999]

fibonacci = fib 1 1
    where
      fib a b = a : fib b (b+a)

euler2 = show . sum . takeWhile (< 4000000) . filter even $ fibonacci

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

euler8 = show . maximum . map product . filter (not . elem 0) . map (take 5)
          . allBut 6 . tails $ input8
