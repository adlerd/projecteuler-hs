module Set7 (set7) where

import Sorted (mergeMany)
import Data.List (minimumBy,sort,foldl')
import EulerUtil (totient,digits)
import Data.Ord (comparing)
import Data.Maybe (fromJust, mapMaybe)
import Data.Ratio ((%),denominator)
import qualified Data.MemoCombinators as MC

set7 = take 10 $ [euler70,euler71,euler72,euler73,euler74] ++ repeat undefined

euler70 = show . fst . fromJust $ foldl' fold Nothing [2..9999999]
    where
      fold m x
          | minimizes && valid = Just (x, ratio)
          | otherwise = m
          where
            minimizes = maybe True ((ratio <) . snd) m
            valid = (sort . digits $ x) == (sort . digits $ t)
            t = totient x
            ratio = fromIntegral x / fromIntegral t

euler71 = show . fst . last . takeWhile ((<= 1000000) . snd) . iterate closer $ (2,7)
    where
      closer (n,d) = head . filter ((> r) . uncurry (%)) . mapMaybe (closest (n+1))
                     $ [d+1..]
          where
            r = n%d
      closest n d = fmap (\n -> (n,d)) . maybeLast . filter gcdone .
                    takeWhile ((< 3%7) . (%d)) $ [n..]
          where
            maybeLast [] = Nothing
            maybeLast xs = Just $ last xs
            gcdone = (1 ==) . gcd d

euler72 = show . sum . map totient $ [2..1000000]

euler73 = show  . sum . map (\d -> length . filter ((d==) . denominator)
                                   . takeWhile (< 1%2) . dropWhile (<= 1%3)
                                   . map (%d) $ [1..d]) $ [5..12000]

euler74 = show . length . filter ((== 60) . chainLen') $ [1..999999]
    where
      fact = MC.arrayRange (0,9) (\d -> product [2..d])
      chainLen _ 169 = 3
      chainLen _ 363601 = 3
      chainLen _ 1454 = 3
      chainLen _ 871 = 2
      chainLen _ 45361 = 2
      chainLen _ 872 = 2
      chainLen _ 45362 = 2
      chainLen f x
          | fdsum == x = 1
          | otherwise = (1 +) . f . sum . map fact . digits $ x
          where
            fdsum = sum . map fact . digits $ x
      chainLen' = MC.arrayRange (1,999999) (chainLen chainLen')
