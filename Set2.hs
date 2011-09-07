module Set2 (set2) where

import Atkin (primes)
import EulerUtil (factors,divisorFun,fibonacci)
import Data.Char (digitToInt, intToDigit)
import Data.Array.Unboxed (listArray,UArray,(!))
import Input (input22)
import qualified Data.Char (ord)
import Data.List (sort,inits,tails,elemIndex,maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import qualified PQ
import Sorted (mapElem)

set2 = [euler20,euler21,euler22,euler23,euler24,euler25,euler26,undefined,undefined,undefined]

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

euler23 = abundantTable `seq` show . sum . filter (not . isAbundantSum) $ [1..28123]
    where
      abundantTable :: UArray Int Bool
      abundantTable = listArray (1,28123) . mapElem [1..28123] $ abundants
      isAbundantSum :: Int -> Bool
      isAbundantSum n = any ((abundantTable !) . (n -)) . takeWhile (< n) $ abundants
      abundants = abundantReverseSieve [1..]
      abundantReverseSieve xs = first : sieve (ins first PQ.Empty) rest
          where
            isAbundant x = (divisorFun 1 x) > (2*x)
            (first:rest) = dropWhile (not . isAbundant) xs
            ins n q = PQ.insert (map (n*) [2..]) q
            sieve _ [] = []
            sieve q@(PQ.Queue (f:fs) _) xx@(x:xs)
                = case compare f x of
                    LT -> sieve (adv q fs) xx
                    GT -> case compare (divisorFun 1 x) (2*x) of
                      LT -> sieve q xs
                      GT -> x : sieve (ins x q) xs
                      EQ -> sieve (ins x q) xs
                    EQ -> f : sieve (adv q fs) xs
            adv q fs = PQ.insert fs $ PQ.deleteMin q

euler24 = map (intToDigit) . (!! 999999) . lexPermutations $ [0..9]
    where
      lexPermutations :: [a] -> [[a]]
      lexPermutations [] = [[]]
      lexPermutations xs = concatMap (\(a,b) -> map (a:) (lexPermutations b))
                           . selectAntiSelect $ xs
          where
            selectAntiSelect xs = zip xs . zipWith (++) (inits xs) . tail . tails $ xs

euler25 = show . fst . head . dropWhile ((< 1000) . length . show . snd) . zip [1..]
          $ fibonacci

euler26 = show . maximumBy (comparing recRep) $ [1..999]
    where
      recRep n = recRep' . product $ threes ++ rest
          where
            (threes,more) = span (3 ==) . dropWhile (2 ==) $ factors n
            rest = dropWhile (5 ==) more
            recRep' 1 = 0
            recRep' n =  (1+) . fromJust . elemIndex start . tail .
                         iterate ((10*) . (`rem` n)) $ start
                where
                  start = head . dropWhile (< n) . iterate (10 *) $ 1
