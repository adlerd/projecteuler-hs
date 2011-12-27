module Set7 (set7) where

import Data.List (minimumBy,sort,foldl')
import EulerUtil (totient,digits)
import Data.Ord (comparing)
import Data.Maybe (fromJust, mapMaybe)
import Data.Ratio ((%))

set7 = take 10 $ [euler70,euler71,euler72] ++ repeat undefined

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
