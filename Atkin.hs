module Atkin (primes) where

import Data.Array.Unboxed
import Data.Maybe (mapMaybe)
import qualified PQ

type Soln = (Int,Int,Int)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] yy = yy
merge xx [] = xx
merge xx@(x:xs) yy@(y:ys)
    | y < x = y : merge xx ys
    | otherwise = x : merge xs yy

mergeMany :: (Ord a) => [[a]] -> [a]
mergeMany [] = []
mergeMany all = head $ until single mm all
    where
      single :: [a] -> Bool
      single [_] = True
      single _ = False
      mm :: (Ord a) => [[a]] -> [[a]]
      mm [] = []
      mm [x] = [x]
      mm (x:y:zs) = (merge x y) : (mm zs)

pairsA,pairsB :: [(Int, [(Int, Int)])]
pairsA = [(d,[(f,g)
              | f <- [1..15], g <- [1..30], 0 == (4*f^2+g^2-d) `rem` 60])
          | d <- [1,13,17,29,37,41,49,53]]
pairsB = [(d,[(f,g)
              | f <- [1..10], g <- [1..30], 0 == (3*f^2+g^2-d) `rem` 60])
          | d <- [7,19,31,43]]
pairsC = [(d,[(f,g)
              | f <- [1..10], g <- [1..30], 0 == (3*f^2-g^2-d) `rem` 60])
          | d <- [11,23,47,59]]

up (x,y,k) = (x,y+30,k+y+15)

type LatticeFun =  (Int,Int) -> Int -> (Int,Int) -> [Int]
latticeA,latticeB,latticeC :: LatticeFun
latticeA (l,b) d (f,g) = concatMap scanUp bottomPoints
    where
      lb = l + b
      bottomPoints :: [Soln]
      bottomPoints = takeWhile (\(x,_,_) -> x > 0) . tail . iterate left
                     . until (\(_,_,k0) -> k0 >= lb) right
                     $ (f,g,(4*f^2+g^2-d) `quot` 60)
          where
            right (x,y0,k0) = (x',y0,k0+x+x')
                where
                  x' = x+15
            left (x,y0,k0) = until (\(_,_,k0) -> k0 >= l) up (x',y0,k0-x-x')
                where
                  x' = x-15
      scanUp :: Soln -> [Int]
      scanUp = takeWhile (< lb) . map (\(_,_,k) -> k) . iterate up
latticeB (l,b) d (f,g) = concatMap scanUp bottomPoints
    where
      lb = l + b
      bottomPoints :: [Soln]
      bottomPoints = takeWhile (\(x,_,_) -> x > 0) . tail . iterate left
                     . until (\(_,_,k0) -> k0 >= lb) right
                     $ (f,g,(3*f^2+g^2-d) `quot` 60)
          where
            right (x,y0,k0) = (x+10,y0,k0+x+5)
            left (x,y0,k0) = until (\(_,_,k0) -> k0 >= l) up (x-10,y0,k0-x+5)
      scanUp :: Soln -> [Int]
      scanUp = takeWhile (< lb) . map (\(_,_,k) -> k) . iterate up
latticeC (l,b) d (f,g) = step2 (f,g,(3*f^2-g^2-d) `quot` 60)
    where
      step2 t@(x,y0,k0)
          | k0 < lb = step4 (y0,k0) t
          | x > y0 = step2 . upC $ t
          | otherwise = []
      upC (x,y,k) = (x,y+30,k-y-15)
      step4 p@(y0,k0) t@(x,y,k)
          | (k >= l && y < x) = k : (step4 p . upC $ t)
          | otherwise = step2 (x+10,y0,k0+x+5)
      lb = l + b

{- pansf: primes and not-square-frees -}
pansfA,pansfB,pansfC :: (Int,Int) -> [Int]
pansfA lb = pansfX lb latticeA pairsA
pansfB lb = pansfX lb latticeB pairsB
pansfC lb = pansfX lb latticeC pairsC
pansfX :: (Int,Int) -> LatticeFun -> [(Int, [(Int, Int)])] -> [Int]
pansfX lb@(l,b) lattice = mergeMany . map pansf'
    where
      pansf' :: (Int,[(Int,Int)]) -> [Int]
      pansf' (d,fgs) = mapMaybe convert $ assocs arr
          where
            convert :: (Int, Bool) -> Maybe Int
            convert (k,b)
                | b = Just (60*k+d)
                | otherwise = Nothing
            arr :: UArray Int Bool
            arr = accumArray (\b _ -> not b) False (l,l+b-1)
                  . map (\k -> (k,())) . concatMap (lattice lb d) $ fgs

pansf lb = mergeMany [pansfA lb, pansfB lb, pansfC lb]

naivePrimes :: [Int]
naivePrimes = sieve [2..]
    where
      sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

squareSieve :: [Int] -> [Int]
squareSieve (first:rest) = first : ss (ins first PQ.Empty) rest
    where
      ss _ [] = []
      ss q@(PQ.Queue (f:fs) _) xx@(x:xs)
          = case compare f x of
              LT -> ss (adv q fs) xx
              GT -> x : ss (ins x q) xs
              EQ -> ss (adv q fs) xs
      adv q fs = PQ.insert fs $ PQ.deleteMin q
      ins p q = PQ.insert (map ((p*p)*) [1..]) q

primes = [2,3,5] ++ (squareSieve $ concatMap pansf [(l,10000) | l <- [0,10000..]])
