{-# LANGUAGE FlexibleContexts #-}

module Set8 (set8) where

import Data.List (mapAccumL,transpose,unfoldr,find,sort)
import EulerUtil (digits)
import Data.Maybe (fromJust,mapMaybe)
import Input (input81,input89)
import qualified PQ
import Data.Array (bounds, (!), (//), Array, listArray)
import Data.Ix (inRange)
import Control.Monad (guard)
import Control.Arrow ((&&&),second)
import Monopoly
import PTriplets (pythags)
import Control.Monad.State.Lazy
import qualified Data.IntMap.Strict as IM
import Atkin (primes)
import Sorted (nub)
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>),(<*>))

set8 :: [(Int, String)]
set8 = zip [80..]
       [euler80,euler81,euler82,euler83,euler84,euler85,euler86,euler87,euler88,
        euler89]

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

type Rect = (Int, Int)

rCount :: Rect -> Int
rCount (a,b) = (a*a+a)*(b*b+b) `quot` 4

area = uncurry (*)

euler85 = show . fst . foldl1 smaller . map (area &&& subtract target . rCount)
          . concat . unfoldr step $ (1,2001)
  where
    target = 2000000
    start = dropWhile ((< target) . rCount) [(1,y) | y <- [1..]]
    over :: Rect -> Rect
    over (x,y) = (x+1,y)
    scanDown :: Rect -> Maybe (Rect,Rect)
    scanDown (x,y) = find ((< target) . rCount . snd) . zip xs $ tail xs
      where
        xs = [(x,y') | y' <- [y,y-1..]]
    step :: Rect -> Maybe ([Rect], Rect)
    step r@(x,y) = do guard (y > x)
                      (h,l) <- scanDown r
                      return ([h,l], over h)
    smaller a'@(_,a) b'@(_,b) = if abs a <= abs b then a' else b'

euler86 = show . fst . head . dropWhile ((< 1000000) . snd) $ runningTotal
  where
    runningTotal = zip fsts . scanl1 (+) $ snds
      where
        (fsts,snds) = unzip totals
    totals = concat . flip evalState IM.empty . mapM updateTriplet $ pythags
    updateTriplet (a,b,_) = do mapM_ updateSingle $ solnsForPy (a,b)
                               removeComplete a
    updateSingle (m,ct) = modify $ IM.insertWith (+) m ct
    removeComplete i = do map <- get
                          let (first@(minK,_),rest) = IM.deleteFindMin map
                          if minK < i
                            then put rest >> fmap (first:) (removeComplete i)
                            else return []
    solnsForPy (x, y) = filter ((> 0) . snd) [sft (x,y), sft (y,x)]
    sft (a, b) = (a, min (a+1) b - b + b `quot` 2)

euler87 = show . length . nub . sort $ sums
  where
    limit = 50000000
    sums = do a <- takeWhile (< limit) $ map (^2) primes
              b <- takeWhile (< limit - a) $ map (^3) primes
              c <- takeWhile (< limit - a - b) $ map (^4) primes
              return (a+b+c)

euler88 = show . sum . nub . sort . IM.elems $ imap
  where
    limit = 12000 :: Int
    resultK :: [Int] -> Int
    resultK xs | product xs >= sum xs = length xs + product xs - sum xs
    extend :: [Int] -> [[Int]]
    extend set = takeWhile ((<= limit) . resultK) $ map (:set) [(head set)..]
    sets :: [[Int]]
    sets = concat . takeWhile (not . null) . iterate (concatMap extend)
           $ [[a] | a <- [2..limit]]
    imap :: IM.IntMap Int
    imap = IM.delete 1 . IM.fromListWith min . map (resultK &&& product) $ sets

euler89 = show $ (sum . map length $ input89) -
                   (sum . map (length . showRN . parseRN) $ input89)

showRN :: Int -> String
showRN = concat . unfoldr nextDigit
nextDigit :: Int -> Maybe (String, Int)
nextDigit n = find ((n >=) . snd) digs >>= return . second (n -)
digs :: [(String, Int)]
digs = map (id &&& parseRN)
         ["M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I"]
parseRN :: String -> Int
parseRN = either (error "Failed to parse RN") id
          . runParser (liftM sum . sequence $ parsers) () "input"
parsers = [sumMany thousand,    option 0 $ subPair hundred thousand,
           sumMany fivehundred, option 0 $ subPair hundred fivehundred,
           sumMany hundred,     option 0 $ subPair ten hundred,
           sumMany fifty,       option 0 $ subPair ten fifty,
           sumMany ten,         option 0 $ subPair one ten,
           sumMany five,        option 0 $ subPair one five,
           sumMany one, eof >> return 0]
      where
        base :: Char -> Int -> Parser Int
        subPair :: Parser Int -> Parser Int -> Parser Int
        sumMany :: Parser Int -> Parser Int
        base c v = char c >> return v
        subPair c1 c2 = try $ subtract <$> c1 <*> c2
        sumMany p = many p >>= return . sum
        [thousand,fivehundred,hundred,fifty,ten,five,one]
          = zipWith base "MDCLXVI" [1000,500,100,50,10,5,1]
