module Set1 (set1) where

import Atkin (primes)
import EulerUtil (slide,divisorCount)
import Data.List (unfoldr,find,maximumBy,foldl1')
import Input (input11, input13)
import Data.Array.IArray ((!), bounds, listArray)
import Data.Array.Unboxed (UArray)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Bits
import Data.Word (Word32)

set1 = [euler10,euler11,euler12,euler13,euler14,undefined,undefined,undefined,undefined,undefined]

euler10 = show . sum . takeWhile (< 2000000) $ primes

euler11 = show . maximum . map product . concatMap (slide 4) $ concat [rows,cols,d1,d2]
    where
      ((rmin,cmin),(rmax,cmax)) = bounds input11
      inX i = input11 ! i
      rows = map (\c -> map (\r -> inX (r,c)) [rmin..rmax]) [cmin..cmax]
      cols = map (\r -> map (\c -> inX (r,c)) [cmin..cmax]) [rmin..rmax]
      d1 = map (\(r0,c0) -> map inX $ zip [r0,r0-1..rmin] [c0..cmax])
           d1'
      d1' = (zip [rmin..rmax] (repeat cmin)) ++ (zip (repeat rmax) [cmin+1..cmax])
      d2 = map (\(r0,c0) -> map inX $ zip [r0..rmax] [c0..cmax])
           d2'
      d2' = (zip [rmax,rmax-1..rmin] (repeat cmin)) ++ (zip (repeat rmin) [cmin+1..cmax])

euler12 = show . fromJust . find ((> 500) . divisorCount) $ scanl1 (+) [1..]

euler13 = take 10 . show . sum $ input13

euler14 = show . fst . foldl1' maxBySnd . map (\x -> (x,collatzLength x)) $ [1..999999]
    where
      maxBySnd aa@(_,a) bb@(_,b)
        | a > b = aa
          | otherwise = bb
      smallLengths :: UArray Word32 Int
      smallLengths = listArray (1,10000) $ 0 : map collatzLength' [2..]
      collatzLength :: Word32 -> Int
      collatzLength n
          | n < 10000 = smallLengths ! n
          | n `testBit` 0 = 2 + (collatzLength $ n + (n `shiftR` 1) + 1)
          | otherwise = 1 + collatzLength (n `shiftR` 1)
      collatzLength' :: Word32 -> Int
      collatzLength' n
          | n `testBit` 0 = if n == 1
                            then 0
                            else 2 + (collatzLength' $ n + (n `shiftR` 1) + 1)
          | otherwise = 1 + collatzLength' (n `shiftR` 1)
