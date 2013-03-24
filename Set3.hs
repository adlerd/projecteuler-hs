module Set3 (set3) where

import EulerUtil (digits,undigits,selectAntiSelect,allBut,isPalindrome,iSqrt)
import Data.List (sort,inits,tails,unfoldr,maximumBy)
import Sorted (nub,mapElem,elem,count)
import Data.Ratio
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Atkin (primes)
import Data.Array.Unboxed (listArray,UArray,(!))
import Control.Monad (guard)
import PTriplets (pythagsBLim)

set3 :: [(Int, String)]
set3 = zip [30..]
       [euler30,euler31,euler32,euler33,euler34,euler35,euler36,euler37,euler38,
       euler39]

euler30 = show . sum . filter (\x -> x == (sum . map (^5) . digits $ x)) $ [2..200000]

euler31 = show . length $ foldr step [(200,[])] [200,100,50,20,10,5,2]
    where
      step :: Int -> [(Int, [Int])] -> [(Int, [Int])]
      step val = concatMap (\(rem,mc) -> takeWhile ((>= 0) . fst)
                                         $  [(rem - val*ct,ct:mc) | ct <- [0..]])

euler32 = show . sum . Sorted.nub . sort . concatMap valid . enumeratePnr 5 $ [1..9]
    where
      valid (p,r) = filter ((r ==) . sort . digits) [ab,cd]
          where
            (a,b) = splitAt 1 p
            ab = undigits a * undigits b
            (c,d) = splitAt 2 p
            cd = undigits c * undigits d
      enumeratePnr c xs = (iterate (concatMap step) [([],xs)]) !! c
          where
            step (p,rem) = map (\(s,as) -> (s:p,as)) . selectAntiSelect $ rem

euler33 = show . denominator . product . mapMaybe valid . concat
          $ [typeA,typeB,typeC,typeD]
    where
      typeA = [([a,x],[b,x],a,b) | a <- [1..9], b <- [1..9], x <- [1..9]]
      typeB = [([x,a],[b,x],a,b) | a <- [0..9], b <- [1..9], x <- [1..9]]
      typeC = [([a,x],[x,b],a,b) | a <- [1..9], b <- [1..9], x <- [1..9]]
      typeD = [([x,a],[x,b],a,b) | a <- [0..9], b <- [1..9], x <- [1..9]]
      valid (ns,ds,n',d')
          | (n < d) && (n % d) == val = Just val
          | otherwise = Nothing
          where
            n = undigits ns
            d = undigits ds
            val = n'%d'

euler34 = show . sum . filter (\x -> x == (sum . map fact . digits $ x))
          $ [3..50000]
    where
      fact n = product [2..n]

euler35 = primeMap `seq` show . length . filter circularPrime . takeWhile (< 1000000)
          $ primes
    where
      circles xs = tail $ zipWith (++) (tails xs) (inits xs)
      numCircles = map undigits . circles . digits
      circularPrime = all (primeMap !) . sort . allBut 1 . numCircles
      primeMap :: UArray Int Bool
      primeMap = listArray (1,999999) $ [1..999999] `Sorted.mapElem` primes

euler36 = show . sum . filter (\x -> (isPalindrome . toBits $ x) &&
                                     (isPalindrome . digits $ x))
          $ [1,3..999999]
    where
      toBits = unfoldr oneBit
      oneBit n
          | n == 0 = Nothing
          | even n = Just (0, n `quot` 2)
          | otherwise = Just (1, n `quot` 2)

euler37 = show . sum . take 11 . filter trunctablePrime . drop 4 $ primes
    where
      trunctablePrime :: Int -> Bool
      trunctablePrime n = all id . (`Sorted.mapElem` primes) . sort . map undigits
                          . concatMap ($ digits n)
                          $ [tail . inits, allBut 1 . tails]

euler38 = show . maximum . concatMap cProds $ [1..10000]
    where
      pandigital :: [Int] -> Bool
      pandigital = ([1..9] ==) . sort
      firstGroup :: (a -> Bool) -> [a] -> [a]
      firstGroup p = takeWhile p . dropWhile (not . p)
      cProds :: Int -> [Int]
      cProds n = map undigits . filter pandigital . firstGroup ((9==) . length)
                 . map (concatMap (digits . (n*))) . drop 2 . inits $ [1..9]

euler39 = show . fst . maximumBy (comparing snd) . count . sort $ relevantPythagPs
    where
      relevantPythagPs = filter (<=1000) . map (\(a,b,c) -> a+b+c)
                         $ pythagsBLim 500
