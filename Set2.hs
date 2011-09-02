module Set2 (set2) where

import EulerUtil (factors,divisorFun)
import Data.Char (digitToInt)
import Data.Array.Unboxed (listArray,UArray,(!))
import Input (input22)
import qualified Data.Char (ord)
import Data.List (sort)
import Data.Maybe (fromJust)

set2 = [euler20,euler21,euler22,undefined,undefined,undefined,undefined,undefined,undefined,undefined]

euler20 = show . sum . map digitToInt . show . product $ [1..100]

euler21 = show . sum . filter valid $ [2..9999]
    where
      aliquotArr :: UArray Int Int
      aliquotArr = listArray (1,9999) . map (\x -> (divisorFun 1 x) - x) $ [1..9999]
      aq n
          | n > 1 && n < 10000 = aliquotArr ! n
          | otherwise = (divisorFun 1 n) - n
      valid n =  aqn /= n && n == aq aqn
          where
            aqn = aq n

euler22 = show . sum . zipWith (*) [1..] . map (sum . map (fromJust . (`lookup` vals)))
          . sort $ input22
    where
      vals = zip ['A'..'Z'] [1..]
