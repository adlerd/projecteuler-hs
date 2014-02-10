module EulerUtil where

import Atkin
import Data.List (unfoldr,tails,inits,foldl')
import Sorted (count, uncount)
import Data.Char (digitToInt,intToDigit)
import Data.Bits
import Data.Either (lefts)

factors x
    | x > 0 = factors' primes x . sqrt . fromIntegral $ x
    where
      factors' _ 1 _ = []
      factors' pp@(p:ps) x qr
          | fromIntegral p > qr = [x]
          | m == 0 = p : (factors' pp d . sqrt . fromIntegral $ d)
          | otherwise = factors' ps x qr
          where
            (d,m) = x `quotRem` p

isPrime x = [x] == factors x

allBut :: Int -> [a] -> [a]
allBut n xs = zipWith (\a b -> a) xs $ drop n xs

iSqrt :: (Ord a, Bits a, Num a) => a -> (a, Bool)
iSqrt 0 = (0, True)
iSqrt n | n >= 0 = head . lefts . iterate (step =<<) $ Right (0, start)
  where
    startable x = (x <= n) && (x > 0)
    start = last . takeWhile startable . iterate (`shiftL` 1) $ 1
    step (root, 0) = Left (root, False)
    step (root, c) = case compare sq n of
                       GT -> Right (root, c')
                       EQ -> Left (root', True)
                       LT -> Right (root', c')
      where
        c' = c `shiftR` 1
        root' = root .|. c
        sq = root' * root'

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

fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)

digits :: (Integral a, Show a) => a -> [Int]
digits = map digitToInt . show
undigits :: [Int] -> Int
undigits = foldl' (\a b -> a*10+b) 0
undigits' :: [Int] -> Integer
undigits' = foldl' (\a b -> a*10+(fromIntegral b)) (0::Integer)

selectAntiSelect xs = zip xs . zipWith (++) (inits xs) . tail . tails $ xs

isPalindrome xs = take n xs == take n (reverse xs)
    where
      n = length xs `quot` 2

rCombinations :: Int -> [a] -> [[a]]
rCombinations 0 xs = [[]]
rCombinations r xs = do (head:rest) <- allBut r . tails $ xs
                        c <- rCombinations (r-1) rest
                        return $ head:c

lengthInRange l u xs
    | l < 0 || u < l = undefined
    | l == 0 = (drop u xs) == []
    | otherwise = firstD /= [] && secD == []
    where
      firstD = drop (l - 1) xs
      secD = drop (u - l + 1) firstD

totient = product . map (\(p,x) -> (p-1)*p^(x-1)) . count . factors

justFind f = head . filter f

maxIn :: Ord b => (a -> b) -> a -> a -> a
maxIn f x y
    | f y > f x = y
    | otherwise = x
