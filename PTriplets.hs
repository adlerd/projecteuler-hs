module PTriplets (pythags, pythagsBLim) where

import qualified PQ
import Data.List (unfoldr,sort)

data Triplet = Triplet { triplet :: !(Int, Int, Int), primitive ::  !(Int, Int, Int) }
  deriving Show

instance Eq Triplet where
  x == y = triplet x == triplet y
instance Ord Triplet where
  x `compare` y = triplet x `compare` triplet y
  x < y = triplet x < triplet y
  x > y = triplet x > triplet y
  x <= y = triplet x <= triplet y
  x >= y = triplet x >= triplet y

isPrimitive :: Triplet -> Bool
isPrimitive x = triplet x == primitive x

nextImprimitive :: Triplet -> Triplet
nextImprimitive (Triplet (a,b,c) p@(a',b',c')) = Triplet (a+a',b+b',c+c') p

pythags :: [(Int, Int, Int)]
pythags = map triplet $ unfoldr p (PQ.insert (Triplet (3,4,5) (3,4,5)) PQ.Empty)
  where
    p :: PQ.Queue Triplet -> Maybe (Triplet, PQ.Queue Triplet)
    p PQ.Empty = Nothing
    p q = Just (min, foldr PQ.insert (PQ.deleteMin q) more)
      where
        min@(Triplet (a,b,c) _) = PQ.findMin q
        more = nextImprimitive min : more'
        more' | isPrimitive min = map (listTrip . mult (tripList min)) $ invUAD
              | otherwise = []
    tripList :: Triplet -> [Int]
    tripList (Triplet (x,y,z) _) = [x,y,z]
    listTrip :: [Int] -> Triplet
    listTrip [x,y,z] = Triplet (x,y,z) (x,y,z)
    mult trip invMat = sort $ map (sum . zipWith (*) trip) invMat
    invUAD = [[[1,-2,2],[2,-1,2],[2,-2,3]],
              [[1,2,2],[2,1,2],[2,2,3]],
              [[-1,2,2],[-2,1,2],[-2,2,3]]]

pythagsBLim :: Int -> [(Int, Int, Int)]
pythagsBLim lim = filter (\(_,y,_) -> y <= lim)
                 . takeWhile (\(x,_,_) -> x <= lim)
                 $ pythags
