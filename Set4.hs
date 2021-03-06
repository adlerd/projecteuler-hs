module Set4 (set4) where

import Atkin (primes)
import Data.List (sort,insert,unfoldr,groupBy)
import EulerUtil (digits,undigits,iSqrt,factors,slide,rCombinations,justFind)
import Sorted (elem,nub,difference,intersection,mergeInfinite,count)
import Input (input42)
import Data.Array.Unboxed (listArray,UArray,(!))

set4 :: [(Int, String)]
set4 = zip [40..]
       [euler40,euler41,euler42,euler43,euler44,euler45,euler46,euler47,euler48,
       euler49]

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

euler43 = show . sum
          $ [undigits [a,b,c,d,e,f,g,h,i,j] |
             d <- [0,2..8],
             f <- [0,5],
             d /= f,
             hij <- [17,34..999],
             let [h,i,j] = if hij < 100
                           then 0 : digits hij
                           else digits hij,
             let five = sort [d,f,h,i,j],
             (5 ==) . length . Sorted.nub $ five,
             g <- [0..9] `difference` five,
             (0 ==) . (`mod` 13) . undigits $ [g,h,i],
             (0 ==) . (`mod` 11) . undigits $ [f,g,h],
             let six = insert g five,
             e <- [0..9] `difference` six,
             (0 ==) . (`mod` 7) . undigits $ [e,f,g],
             let seven = insert e six,
             c <- [0..9] `difference` seven,
             (0 ==) . (`mod` 3) . undigits $ [c,d,e],
             let two = [0..9] `difference` (insert c seven),
             [a,b] <- [two,reverse two],
             a /= 0]

euler44 = show . head $ [d |
                         j <- pentagonals,
                         d <- takeWhile (j >) pentagonals,
                         let k = j-d,
                         isPentagonal k,
                         isPentagonal $ j+k]
    where
      isPentagonal :: Int -> Bool
      isPentagonal n = case iSqrt (24*n+1) of
                         (b,True) -> b `rem` 6 == 5
                         _ -> False
pentagonals = scanl (+) 1 [4,7..]
triangles = scanl (+) 1 [2..]
hexagonals = scanl (+) 1 [5,9..]

euler45 = show . head . foldl1 intersection $ [h', p', t']
    where
      back = dropWhile (<= 40755)
      p' = back pentagonals
      t' = back triangles
      h' = back hexagonals

euler46 = show . head . flip Sorted.difference primes . Sorted.difference [3,5..]
          . mergeInfinite . map (\p -> map ((p +) . (2 *) . (^ 2)) [0..]) $ primes

euler47 = show . fst . justFind (and . snd) . zip [1..] . slide 4
        . map ((> 3) . length . count . factors) $ [1..]

euler48 = reverse . take 10 . reverse . show . sum . map (\x -> x^x) $ [1..1000]

euler49 = justFind (/= "148748178147") . map (concatMap show) . filter isArithSeq
          . concatMap (rCombinations 3) . map (map snd)
          . groupBy (\a b -> fst a == fst b) . sort
          . map (\p -> (read . sort . show $ p :: Int, p))
          . takeWhile (< 10000) . dropWhile (< 1000) $ primes
    where
      isArithSeq [a,b,c] = c == b + (b - a)
