module Set8 (set8) where

import Data.List (mapAccumL,transpose)
import EulerUtil (digits)
import Data.Maybe (fromJust,mapMaybe)
import Input (input81)
import qualified PQ
import Data.Array (bounds, (!), (//), Array, listArray)
import Data.Ix (inRange)

set8 :: [(Int, String)]
set8 = zip [80..]
       [euler80,euler81,euler82,euler83]

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

makeRows n xs = takeWhile ([] /=) . tail . map fst . iterate (splitAt n . snd) $ ([], xs)

aRow mLeft (mUp,this) = Just $ this + mMin mUp mLeft
mMin Nothing (Just x) = x
mMin (Just x) Nothing = x
mMin Nothing Nothing = 0
mMin (Just x) (Just y) = min x y

input81' = makeRows 80 input81
euler81 = show . fromJust . last . foldl nextRow (repeat Nothing) $ input81'
    where
      nextRow prev current = tail . scanl aRow Nothing $ zip prev current

euler82 = show . foldl1 min . last . scanl1 nextRow . transpose $ input81'
    where
      nextRow prev current = zipWith mMin
                             (tail $ scanl aRow Nothing pc)
                             (init $ scanr (flip aRow) Nothing pc)
          where
            pc = zip (map Just prev) current

-- problem 83: Dijkstra's algorihm
data DState = DState { source :: Array (Int, Int) (Maybe Int)
                     , unvisited :: PQ.Queue (Int, (Int, Int)) }
             | DDone Int

dijStep :: DState -> DState
dijStep (DDone i) = DDone i
dijStep (DState src ud)
    | nextPlace == snd arrBounds = DDone nextVal
    | alreadyVisited =  DState src newpq --ignore repeated entry
    | otherwise = DState (src // [(nextPlace, Nothing)]) --mark visited
                  . foldr PQ.insert newpq $ neighbors -- add tentative values
    where
      arrBounds = bounds src
      alreadyVisited = Nothing == src ! nextPlace
      (nextVal, nextPlace@(npx,npy)) = PQ.findMin ud
      newpq = PQ.deleteMin ud
      tentative xy = do if (inRange arrBounds xy) then Just () else Nothing
                        srcVal <- src ! xy
                        return (nextVal + srcVal, xy)
      neighbors = mapMaybe tentative
                  $ [(npx,npy+1),(npx,npy-1),(npx+1,npy),(npx-1,npy)]

euler83 = show . head . mapMaybe stateToMaybe . iterate dijStep $ start
    where
      input :: Array (Int, Int) (Maybe Int)
      input = (listArray ((1,1),(80,80)) . map (Just . fromIntegral) $ input81)
      stateToMaybe (DDone i) = Just i
      stateToMaybe _ = Nothing
      start = DState input (PQ.insert (fromJust $ input ! st, st) PQ.Empty)
      st = fst . bounds $ input
