module Set6 (set6) where

import EulerUtil (digits,undigits,isPrime)
import Atkin (primes)
import Data.List (tails,permutations)

set6 = take 10 $ [euler60,euler61] ++ repeat undefined

euler60 = show . head $ do (a:as) <- tails primes'
                           let a' = filter (goodpair a) as
                    	   (b:bs) <- tails a'
                    	   let b' = filter (goodpair b) bs
                    	   (c:cs) <- tails b'
                    	   let c' = filter (goodpair c) cs
                    	   (d:ds) <- tails c'
                    	   let d' = filter (goodpair d) ds
                    	   e <- d'
                    	   return (sum [a,b,c,d,e])
    where
      goodpair x y = x /= y && isP xd yd && isP yd xd
          where
            xd = digits x
            yd = digits y
            isP a b = isPrime . undigits $ a ++ b
      primes' = 3 : (takeWhile (< 10000) . dropWhile (<= 5) $ primes)

euler61 = head $
          do order <- permutations . map (takeWhile (< 10000) . dropWhile (< 1000))
                      . take 6 . map (\n -> scanl1 (+) [1,n-1..]) $ [3..]
             a <- (order !! 0)
             b <- filter (links a) (order !! 1)
             c <- filter (links b) (order !! 2)
             d <- filter (links c) (order !! 3)
             e <- filter (links d) (order !! 4)
             f <- filter (links e) (order !! 5)
             if links f a
             then return . show . sum $ [a,b,c,d,e,f]
             else fail ""
    where
      links p = ((== (p `mod` 100)) . (`div` 100))
