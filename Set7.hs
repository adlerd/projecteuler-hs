module Set7 (set7) where

import Data.List (minimumBy,sort,foldl')
import EulerUtil (totient,digits)
import Data.Ord (comparing)
import Data.Maybe (fromJust)

set7 = take 10 $ [euler70] ++ repeat undefined

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
