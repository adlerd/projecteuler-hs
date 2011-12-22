module Set6 (set6) where

import EulerUtil (digits,undigits,isPrime,lengthInRange)
import Atkin (primes)
import Data.List (tails,permutations,unfoldr,sort,groupBy,sortBy,elemIndex,findIndex,
                  find,maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust,mapMaybe)

set6 = take 10 $ [euler60,euler61,euler62,euler63,euler64,euler65,euler66] ++ repeat undefined

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

euler62 = show . fst . head . head . filter (lengthInRange 5 5)
          . concatMap (groupBy sndEq . sortBy (comparing snd)
                       . map (\x -> (x, sort . digits $ x))) $ cubesByLen
    where
      sndEq (_,a) (_,b) = a == b
      cubesByLen = unfoldr (\(lim, xs) -> Just . (\(a,b) -> (a,(10*lim,b)))
                                          . span (< lim) $ xs)
                   (10, map (^3) [1..])

euler63 = show . length . concat . takeWhile (not . null)
          . map (\(n,ps) -> dropWhile (< 10^(n-1)) . takeWhile (< 10^n) $ ps)
          . map nthPow $ [1..]
    where
      nthPow n = (n, map (^n) [1..9])

floorSqrt n = fromIntegral . fromJust . findIndex (> n) . map (\n -> n * n) $ [1..]

findExpansionTerms sq
    | flsq * flsq == sq = []
    | otherwise = iterate (fet flsq) (0,1,flsq)
    where
      fet a0 (m,d,a) = (m1,d1,a1)
          where
            m1 = d*a-m
            d1 = (sq-m1^2) `div` d
            a1 = (a0+m1) `div` d1
      flsq = floorSqrt sq

contFracConv as = (aa as,bb as)
    where
      rel = zipWith3 (\l1 l2 bn -> bn * l1 + l2)
      aa b = drop 2 aa'
          where
            aa' = 0 : 1 : rel (tail aa') aa' as
      bb b = drop 2 bb'
          where
            bb' = 1 : 0 : rel (tail bb') bb' as

euler64 = show . length . filter odd . map period $ [1..10000]
    where
      period n = case findExpansionTerms n of
                   [] -> 0
                   (_:f:rest) -> (1 +) . fromJust . elemIndex f $ rest

euler65 = show . sum . digits . (!! 99) . fst . contFracConv
          $ 2 : concatMap (\n -> [1,2*n,1]) [1..]

euler66 = show . fst . maximumBy (comparing snd) . mapMaybe minX $ [2..1000]
    where
      minX :: Integer -> Maybe (Integer, Integer)
      minX d = do et <- if null expTerms then Nothing else Just expTerms
                  s <- find solves . uncurry zip . contFracConv $ et
                  return $ (d, fst s)
          where
            expTerms = map (\(_,_,a) -> a) . findExpansionTerms $ d
            solves (h,k) = h^2 - d*k^2 == 1
