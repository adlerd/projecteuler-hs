module Set7 (set7) where

import Sorted (mergeMany,mergeInfinite,nub)
import Data.List (minimumBy,sort,foldl',nub,groupBy,sortBy)
import EulerUtil (totient,digits,lengthInRange,isPrime,undigits)
import Data.Ord (comparing)
import Data.Maybe (fromJust, mapMaybe)
import Data.Ratio ((%),denominator)
import qualified Data.MemoCombinators as MC
import Input (input79)

set7 :: [(Int, String)]
set7 = zip [70..]
       [euler70,euler71,euler72,euler73,euler74,euler75,euler76,euler77,euler78,
       euler79]

euler70 = show . fst . fromJust $ foldl' fold Nothing [2..9999999]
    where
      fold m x
          | minimizes && valid = Just (x, ratio)
          | otherwise = m
          where
            minimizes = maybe True ((ratio <) . snd) m
            valid = (sort . digits $ x) == (sort . digits $ t)
            t = totient x
            ratio = fromIntegral x / fromIntegral t

euler71 = show . fst . last . takeWhile ((<= 1000000) . snd) . iterate closer $ (2,7)
    where
      closer (n,d) = head . filter ((> r) . uncurry (%)) . mapMaybe (closest (n+1))
                     $ [d+1..]
          where
            r = n%d
      closest n d = fmap (\n -> (n,d)) . maybeLast . filter gcdone .
                    takeWhile ((< 3%7) . (%d)) $ [n..]
          where
            maybeLast [] = Nothing
            maybeLast xs = Just $ last xs
            gcdone = (1 ==) . gcd d

euler72 = show . sum . map totient $ [2..1000000]

euler73 = show  . sum . map (\d -> length . filter ((d==) . denominator)
                                   . takeWhile (< 1%2) . dropWhile (<= 1%3)
                                   . map (%d) $ [1..d]) $ [5..12000]

euler74 = show . length . filter ((== 60) . chainLen') $ [1..999999]
    where
      fact = MC.arrayRange (0,9) (\d -> product [2..d])
      chainLen _ 169 = 3
      chainLen _ 363601 = 3
      chainLen _ 1454 = 3
      chainLen _ 871 = 2
      chainLen _ 45361 = 2
      chainLen _ 872 = 2
      chainLen _ 45362 = 2
      chainLen f x
          | fdsum == x = 1
          | otherwise = (1 +) . f . sum . map fact . digits $ x
          where
            fdsum = sum . map fact . digits $ x
      chainLen' = MC.arrayRange (1,999999) (chainLen chainLen')

euler75 = show . length . filter (lengthInRange 1 1 . Data.List.nub . map snd)
          . groupBy (\a b -> fst a == fst b) . map (\(s,abc) -> (s`div`2,abc))
          . takeWhile ((<= 1500000) . fst) . mergeInfinite . map gtsM $ [1..]
    where
      genTrip mSq mDb n = (a'+b'+c,(a,b,c))
          where
            a' = mSq - n^2
            b' = mDb*n
            c = mSq + n^2
            (a,b) = if a' > b' then (b',a') else (a',b')
      gtsM m = mergeInfinite
               . map (flip map [1..] . mult . genTrip sq db)
               . filter ((1 ==) . gcd m) $ [1..m-1]
          where
            sq = m * m
            db = m + m
            mult (s,(a,b,c)) k = (s*k,(a*k,b*k,c*k))

euler76 = show . subtract 1 $ parts' 100 1
    where
      parts f n k
            | k > n = 0
            | k == n = 1
            | otherwise = (f n $ k+1) + (f (n-k) k)
      parts' = MC.memo2 (MC.arrayRange (1,100)) MC.integral (parts parts')

euler77 = show . fst . head . dropWhile ((<= 5000) . snd) . map (\n -> (n, parts' n 1))
          $ [1..]
    where
      parts f n k
          | k > n = 0
          | k == n = if isPrime n then 1 else 0
          | otherwise = (f n $ k+1) + if isPrime k then (f (n-k) k) else 0
      parts' = MC.memo2 (MC.arrayRange (1,200)) MC.integral (parts parts')

euler78 = show . fst . head . dropWhile ((/= 0) . (`mod` 1000000) . snd)
            . map (\n -> (n, parts' n)) $ [1..]
    where
      parts :: (Int -> Integer) -> Int -> Integer
      parts _ 0 = 1
      parts f n = foldl sumMod 0 . zipWith (*) pairsTimes . map f
                  . takeWhile (>= 0) . map (n -) $ genPent
      parts' = MC.chunks MC.arrayRange (next 0) (parts parts')
          where
            step = 10000
            next st = (st, st+step) : next (st + step + 1)
      sumMod :: Integer -> Integer -> Integer
      sumMod a b = if (abs ans) > 10000000 then ans `mod` 1000000 else ans
          where ans = a + b
      genPent = concatMap (\k -> [k*(3*k-1)`quot`2,k*(3*k+1)`quot`2]) $ [1..]
      pairsTimes = 1 : 1 : -1 : -1 : pairsTimes

euler79 = show . undigits . head
          . foldr (\x-> nubgroupsort . concatMap (rectify x)) [[]] $ input79
    where
      nubgroupsort = Sorted.nub . head .groupBy (\a b -> length a == length b)
                     . sortBy (comparing length)
      rectify :: (Eq a) => [a] -> [a] -> [[a]]
      rectify a b = filter good $ rectify' a b
          where
            good [] = True
            good [_] = True
            good (x:y:_) = x /= y
            rectify' :: (Eq a) => [a] -> [a] -> [[a]]
            rectify' [] ys = [ys]
            rectify' xs [] = [xs]
            rectify' xx@(x:xs) yy@(y:ys)
                | x == y = normal ++ (map (x:) $ rectify xs ys)
                | otherwise = normal
                where
                  normal = left ++ right
                  left = map (x:) $ rectify xs yy
                  right = map (y:) $ rectify xx ys
