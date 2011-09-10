module Set3 (set3) where

import EulerUtil (digits,undigits,selectAntiSelect,allBut)
import Data.List (sort,inits,tails)
import Sorted (nub,mapElem)
import Data.Ratio
import Data.Maybe (mapMaybe)
import Atkin (primes)
import Data.Array.Unboxed (listArray,UArray,(!))

set3 = take 10 $ [euler30,euler31,euler32,euler33,euler34,euler35] ++ repeat undefined

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
