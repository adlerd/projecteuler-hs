module Set5 (set5) where

import Input (input54,input59)
import Sorted (elem,mergeInfinite,nub,mapElem)
import Atkin (primes)
import Data.List (sortBy,tails,unfoldr,foldl',groupBy,sort)
import Data.Ord (comparing)
import EulerUtil (digits,slide,isPalindrome,undigits,undigits',isPrime)
import Data.Maybe (mapMaybe,listToMaybe,catMaybes)
import Data.Char (digitToInt,chr)
import Data.Ratio
import Data.Bits (xor)

set5 = [euler50,euler51,euler52,euler53,euler54,euler55,euler56,euler57,euler58,euler59]

euler50 = show . fst . head . filter (flip Sorted.elem primes . fst)
          . sortBy (flip $ comparing snd) . takeWhile ((< 1000000) . fst)
          $ primeSpanSums
    where
      primeSpanSums = mergeInfinite
                      . map (\(h:t) -> flip zip [1..] . map (subtract h) $ t)
                      .  tails . scanl (+) 0 $ primes

euler51 = show . head . concatMap selByG8 . bylog10 $ primes
    where
      selByG8 = sort . map (snd . head) . filter ((8 ==) . length)
                . groupBy (\a b -> fst a == fst b) . sortBy (comparing fst)
                . concatMap classifyPrime
      bylog10 xs = unfoldr step (10,xs)
          where
            step (_,[]) = Nothing
            step (u,xs) = Just (s1, (10*u,s2))
                where
                  (s1,s2) = span (< u) xs
      classifyPrime p = filter (\((_,b),_) -> b /= 0)
                        . map (\(a,b,_) -> (((foldl' b10 0 a)*10+o, foldr b2 0 b),p))
                        $ trips
          where
            trips = foldr concatMap [([],[],Nothing)] . map step . digits $ p'
            (p', o) = p `quotRem` 10
            step d (a,b,mc) = (d:a,False:b,mc) : (step' d a b mc)
            step' d a b Nothing = [(a,True:b,Just d)]
            step' d a b jc@(Just c)
                | d == c = [(a,True:b,jc)]
                | otherwise = []
            b10 r d = r*10+d
            b2 True r = r*2+1
            b2 False r = r*2

euler52 = show . head . filter satisfies $ [1..]
    where
      satisfies x = all ((ds==) . sort . digits . (x*)) [2..6]
          where
            ds = sort . digits $ x
euler53 = show . length . filter (> 1000000) . concat . take 100 . tail $ pascal
    where
      pascal = map (takeWhile (>0)) . iterate (\xs -> zipWith (+) xs $ 0:xs)
               $ 1 : repeat 0

data HandType = NoPair
              | OnePair Int
              | TwoPair Int Int -- must be TwoPair high low
              | ThreeKind Int
              | Straight --will be compared on high card
              | Flush -- will be compared on high card
              | FullHouse Int Int
              | FourKind Int
              | StraightFlush --will be compared on high card; == royal flush
                deriving (Eq, Ord, Show)

evaluateHand :: [(Int, Char)] -> (HandType, [Int])
evaluateHand xs = (htype, Sorted.nub vsort)
    where
      boolToMaybe True v = Just v
      boolToMaybe False _ = Nothing
      htype = head . catMaybes $ [boolToMaybe (straight && flush) StraightFlush,
                                  Just . FourKind =<< (groupFind . slide 4 $ vsort),
                                  fullHouseFind,
                                  boolToMaybe flush Flush,
                                  boolToMaybe straight Straight,
                                  Just . ThreeKind =<< threes,
                                  boolToMaybe (2 == length pairs)
                                                  (TwoPair (pairs !! 0) (pairs !! 1)),
                                  boolToMaybe (1 == length pairs)
                                                  (OnePair . head $ pairs),
                                  Just NoPair]
      downfrom x = [x,x-1..]
      straight = vsort == (take 5 . downfrom . head $ vsort)
                 || vsort == [14,5,4,3,2]
      alleq (x:xs) = all (x ==) xs
      flush = alleq . map snd $ xs
      pairs = map head . filter alleq . slide 2 $ vsort
      vsort = reverse . sort . map fst $ xs
      threes = groupFind . slide 3 $ vsort
      groupFind gs = do g <- listToMaybe (filter alleq gs)
                        return $ head g
      fullHouseFind = do t <- threes
                         p <- listToMaybe $ filter (t /=) pairs
                         return $ FullHouse t p

euler54 = show . length . filter (winOne . splitAt 5 . map readcard) . takeWhile (/=[])
          . unfoldr (Just . splitAt 10) . words $ input54
    where
      winOne (a,b) =  (evaluateHand a) > (evaluateHand b)
      readcard [v,s] = (\v' -> (v',s)) $ case v of
                                           'T' -> 10
                                           'J' -> 11
                                           'Q' -> 12
                                           'K' -> 13
                                           'A' -> 14
                                           otherwise -> digitToInt v

euler55 = show . length . filter lychrel $ [1..9999]
    where
      lychrel = ([] ==) . filter (isPalindrome . show) . take 50 . tail . iterate step
      step x = x + (undigits' . reverse . digits $ x)

euler56 = show . maximum . map (sum . digits) $ [a^b | a <- [1..100], b <- [1..100]]

euler57 = show . length . filter satisfies . take 1000 . iterate step $ 3%2
    where
      step r = (nd+d)%(nd)
          where
            nd = n + d
            n = numerator r
            d = denominator r
      satisfies r = (length . digits $ n) > (length . digits $ d)
          where
            n = numerator r
            d = denominator r

euler58 = show . (\(_,_,n,_) -> 2*n+1) . head . dropWhile (\(r,_,_,_) -> r >= 1%10)
          . iterate step $ (1%1,0,0,diagPrimes)
    where
      diags = concat [[x,x+2*n,x+4*n]| n <- [1..], let x = 1+2*n*(2*n-1)]
      diagPrimes = filter isPrime diags
      step (_,pCt,nPrev,dps) = (pCt'%totalCt,pCt',n,dps2)
          where
            n = nPrev + 1
            totalCt = 4*n+1
            (dps1,dps2) = span (< (2*n+1)^2) dps
            pCt' = pCt + length dps1

euler59 = show . sum . head . filter (\t -> all (`Prelude.elem` (asciiWords t)) common)
          . map (decrypt input59)
          $ [[a,b,c] | a <- [97..122], b <- [97..122], c <- [97..122]]
    where
      decrypt ct pw = zipWith xor ct $ cycle pw
      asciiWords = words . map chr
      common = ["the","and","that"]
