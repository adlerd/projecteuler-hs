module Set5 (set5) where

import Sorted (elem,mergeInfinite)
import Atkin (primes)
import Data.List (sortBy,tails)
import Data.Ord (comparing)

set5 = take 10 $ [euler50] ++ repeat undefined

euler50 = show . fst . head . filter (flip Sorted.elem primes . fst)
          . sortBy (flip $ comparing snd) . takeWhile ((< 1000000) . fst)
          $ primeSpanSums
    where
      primeSpanSums = mergeInfinite
                      . map (\(h:t) -> flip zip [1..] . map (subtract h) $ t)
                      .  tails . scanl (+) 0 $ primes
