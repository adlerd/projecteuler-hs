module Set5 (set5) where

import Sorted (elem,mergeInfinite)
import Atkin (primes)
import Data.List (sortBy,tails,unfoldr,foldl',groupBy,sort)
import Data.Ord (comparing)
import EulerUtil (digits)

set5 = take 10 $ [euler50,euler51,euler52,euler53] ++ repeat undefined

euler50 = show . fst . head . filter (flip Sorted.elem primes . fst)
          . sortBy (flip $ comparing snd) . takeWhile ((< 1000000) . fst)
          $ primeSpanSums
    where
      primeSpanSums = mergeInfinite
                      . map (\(h:t) -> flip zip [1..] . map (subtract h) $ t)
                      .  tails . scanl (+) 0 $ primes

euler51 = show . head . concatMap selByG8 . bylog10 $ primes
    where
      selByG8 = sort . map (snd . head) . filter ((8 ==) . length)
                . groupBy (\a b -> fst a == fst b) . sortBy (comparing fst)
                . concatMap classifyPrime
      bylog10 xs = unfoldr step (10,xs)
          where
            step (_,[]) = Nothing
            step (u,xs) = Just (s1, (10*u,s2))
                where
                  (s1,s2) = span (< u) xs
      classifyPrime p = filter (\((_,b),_) -> b /= 0)
                        . map (\(a,b,_) -> (((foldl' b10 0 a)*10+o, foldr b2 0 b),p))
                        $ trips
          where
            trips = foldr concatMap [([],[],Nothing)] . map step . digits $ p'
            (p', o) = p `quotRem` 10
            step d (a,b,mc) = (d:a,False:b,mc) : (step' d a b mc)
            step' d a b Nothing = [(a,True:b,Just d)]
            step' d a b jc@(Just c)
                | d == c = [(a,True:b,jc)]
                | otherwise = []
            b10 r d = r*10+d
            b2 True r = r*2+1
            b2 False r = r*2

euler52 = show . head . filter satisfies $ [1..]
    where
      satisfies x = all ((ds==) . sort . digits . (x*)) [2..6]
          where
            ds = sort . digits $ x
euler53 = show . length . filter (> 1000000) . concat . take 100 . tail $ pascal
    where
      pascal = map (takeWhile (>0)) . iterate (\xs -> zipWith (+) xs $ 0:xs)
               $ 1 : repeat 0
