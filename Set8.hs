module Set8 (set8) where

import Data.List (mapAccumL)
import EulerUtil (digits)

set8 = take 10 $ [euler80] ++ repeat undefined

euler80 = show . sum . concat . filter (\(_:xs) -> xs /= replicate 99 0)
          . map sqrtDigs $ [1..99]
    where
      sqrtDigs n = take 100 $ digs
          where
            digs = snd . mapAccumL sqrtDigs' (0,0,0) $ digitsByTwo
            digitsByTwo = byTwo (digits n) ++ repeat (0,0)
      sqrtDigs' :: (Integer, Integer, Integer) -> (Int,Int) ->
                   ((Integer, Integer, Integer), Integer)
      sqrtDigs' (x, y, r) (alpha1,alpha2) = ((x', y', r'), beta)
          where
            beta = last . filter (\b -> (10*y+b)^2 <= x') $ [0..9]
            x' = 100*x + fromIntegral (10*alpha1+alpha2)
            y' = (10*y+beta)
            r' = x' - y'*y'
      byTwo xs = if even (length xs) then byTwo' xs else byTwo' (0:xs)
      byTwo' [] = []
      byTwo' (a:b:xs) = (a,b) : byTwo xs
