module Set4 (set4) where

import Atkin (primes)
import Data.List (sort)
import EulerUtil (digits)
import Sorted (elem)
import Input (input42)
import Data.Array.Unboxed (listArray,UArray,(!))

set4 = take 10 $ [euler40,euler41,euler42] ++ repeat undefined

euler40 = show . product . map ((intsCat !!) . (10^)) $ [0..6]
    where
      intsCat = concatMap digits [0..]

euler41 = show . last . filter (pandigital . digits) . takeWhile (< 8000000) $ primes
    where
      pandigital xs = ([1..(length xs)] ==) . sort $ xs

euler42 = show . length . filter (`Sorted.elem` triangleNums) . map (sum . map letterVal)
          $ input42
    where
      triangleNums = scanl (+) 0 [1..]
      letterVal = (letters !)
      letters :: UArray Char Int
      letters = listArray ('A','Z') [1..]
