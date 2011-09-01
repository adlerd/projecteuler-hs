module Set1 (set1) where

import Atkin (primes)
import EulerUtil (slide,divisorCount)
import Data.List (unfoldr,find)
import Input (input11, input13)
import Data.Array.IArray ((!), bounds)
import Data.Maybe (fromJust)

set1 = [euler10,euler11,euler12,euler13,undefined,undefined,undefined,undefined,undefined,undefined]

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
