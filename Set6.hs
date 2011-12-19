module Set6 (set6) where

import EulerUtil (digits,undigits,isPrime)
import Atkin (primes)
import Data.List (tails)

set6 = take 10 $ [euler60] ++ repeat undefined

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
