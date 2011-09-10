module EulerUtil where

import Atkin
import Data.List (unfoldr, tails,inits)
import Sorted (count, uncount)
import Data.Char (digitToInt,intToDigit)

factors x
    | x > 0 = factors' primes x
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

iSqrt x = if (guess * guess) == x then Just guess else Nothing
    where
      guess = round . sqrt . fromIntegral $ x

slide :: Int -> [a] -> [[a]]
slide n = map (take n) . allBut n . tails

divisorCount n
    | n > 1 = (product . map ((1 +) . snd) . count . factors $ n)
    | n == 1 = 1
    | otherwise = undefined

divisorFun 0 = divisorCount
divisorFun 1 = sum . divisors
divisorFun n = sum . map (^ n) . divisors 

divisors n = map (product . uncount . zip pfs) . enumerateMCs $ fcs
    where
      (pfs,fcs) = unzip . count . factors $ n
      enumerateMCs = foldr (\ct mcs -> concatMap (\mc -> map (:mc) [0..ct]) mcs) [[]]

fibonacci = fib 1 1
    where
      fib a b = a : fib b (b+a)

digits :: (Integral a) => a -> [Int]
digits = map digitToInt . show
undigits :: (Integral a, Read a) => [Int] -> a
undigits = read . map intToDigit

selectAntiSelect xs = zip xs . zipWith (++) (inits xs) . tail . tails $ xs

isPalindrome xs = take n xs == take n (reverse xs)
    where
      n = length xs `quot` 2
