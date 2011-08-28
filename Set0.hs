module Set0 where

import Sorted (union, count, uncount)
import EulerUtil (factors, by)
import Data.List (sort, groupBy, maximumBy)
import Data.Ord (comparing)

set0 = [undefined,euler1,euler2,euler3,euler4,euler5]

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
