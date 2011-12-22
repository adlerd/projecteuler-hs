module Set1 (set1,reduceTri) where

import Atkin (primes)
import EulerUtil (slide,divisorCount)
import Data.List (unfoldr,find,maximumBy,foldl1')
import Input (input11, input13, input18)
import Data.Array.IArray ((!), bounds, listArray)
import Data.Array.Unboxed (UArray)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Bits
import Data.Word (Word32)
import Data.Char (digitToInt)

set1 = [euler10,euler11,euler12,euler13,euler14,euler15,euler16,euler17,euler18,euler19]

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

euler15 = show $ (product [21..40]) `quot` (product [1..20])
        {- This is 21 choose 20 with repetitions; an array of right-movements
        leaving 21 positions among and around them for down-movements to fit: 21
        boxes and 20 balls. This is C(21+20-1,20), which is (40!)/(20!20!) -}

euler16 = show . sum . map digitToInt . show $ 2^1000

euler17 = show
          $ hundred * 10 --the tens and ones "digits"
          + ten * 100 -- the hundreds "digits"
          + (length "hundred") * 900
          + (length "and") * (900 - 9)
          + (length "onethousand")
          
    where
      ten = sum . map length
            $ ["","one","two","three","four","five","six","seven","eight","nine"]
      teens = sum . map length
              $ ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen",
                 "seventeen","eighteen","nineteen"]
      tenswords = sum . map length $
                  ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
      hundred = ten*9 + teens + tenswords*10

euler18 = show $ reduceTri input18

reduceTri = head . foldr1 (\top bottom -> zipWith (+) top (reduceRow bottom))
    where
      reduceRow :: (Ord a) => [a] -> [a]
      reduceRow = map (\[a,b] -> max a b) . slide 2

euler19 = show . length . filter (\(w,(d,_,_)) -> w == SUN && d == 1) .
          takeWhile (\(_,(_,_,y)) -> y < 2001) . dropWhile (\(_,(_,_,y)) -> y < 1901)
          $ from1900

data Months = JAN | FEB | MAR | APR | MAY | JUN | JUL | AUG | SEP | OCT | NOV | DEC
              deriving (Enum, Ord, Eq, Show)

expandYear year = concatMap (expandMonth year) [JAN .. DEC]
expandMonth year month = map (\d -> (d,month,year)) [1..days]
    where
      days
          | month `elem` [JAN, MAR, MAY, JUL, AUG, OCT, DEC] = 31
          | month `elem` [APR, JUN, SEP, NOV] = 30
          | year `rem` 4 /= 0 || (year `rem` 400) `elem` [100,200,300] = 28
          | otherwise = 29

data Days = MON | TUE | WED | THU | FRI | SAT | SUN
            deriving (Enum, Ord, Eq, Show)

from1900 = zip (cycle [MON .. SUN]) $ concatMap expandYear [1900..]
