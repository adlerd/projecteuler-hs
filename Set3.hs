module Set3 (set3) where

import EulerUtil (digits,undigits,selectAntiSelect)
import Data.List (sort)
import Sorted (nub)

set3 = take 10 $ [euler30,euler31,euler32] ++ repeat undefined

euler30 = show . sum . filter (\x -> x == (sum . map (^5) . digits $ x)) $ [2..200000]

euler31 = show . length $ foldr step [(200,[])] [200,100,50,20,10,5,2]
    where
      step :: Int -> [(Int, [Int])] -> [(Int, [Int])]
      step val = concatMap (\(rem,mc) -> takeWhile ((>= 0) . fst)
                                         $  [(rem - val*ct,ct:mc) | ct <- [0..]])

euler32 = show . sum . Sorted.nub . sort . concatMap valid . enumeratePnr 5 $ [1..9]
    where
      valid (p,r) = filter ((r ==) . sort . digits) [ab,cd]
          where
            (a,b) = splitAt 1 p
            ab = undigits a * undigits b
            (c,d) = splitAt 2 p
            cd = undigits c * undigits d
      enumeratePnr c xs = (iterate (concatMap step) [([],xs)]) !! c
          where
            step (p,rem) = map (\(s,as) -> (s:p,as)) . selectAntiSelect $ rem
