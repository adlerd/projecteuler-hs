module Set9 (set9) where

import EulerUtil (rCombinations, digits)
import Data.List (sort,permutations,maximumBy)
import Control.Monad (filterM)
import Control.Monad.State.Lazy
import qualified Data.IntMap.Strict as IM
import qualified Sorted
import Data.Ratio
import Control.Arrow ((&&&))
import Data.Ord (comparing)

set9 :: [(Int, String)]
set9 = zip [90..]
       [euler90,euler91,euler92,euler93]

euler90 = show . length . filter valid . pairs $ cubes
  where
    cubes = rCombinations 6 [0..9]
    pairs [] = []
    pairs (x:xs) = map ((,) x) xs ++ pairs xs
    valid (a,b) = all (makes . digits . (^2)) [1..9]
      where
        makes [x] = makes [0,x]
        makes [x,y] = x `elem'` a && y `elem'` b || x `elem'` b && y `elem'` a
    elem' 6 l = elem' 9 l
    elem' 9 l = elem 9 l || elem 6 l
    elem' x l = elem x l

euler91 = show . length . filter valid . pairs $ points
  where
    pairs [] = []
    pairs (x:xs) = map ((,) x) xs ++ pairs xs
    points = tail [ (x,y) | x <- [0..50], y <- [0..50] ]
    valid (p1@(x1,y1),p2@(x2,y2)) = side1+side2 == side3
      where
        [side1,side2,side3] = sort [len p1,len p2,len (x1-x2,y1-y2)]
        len (x,y) = x*x+y*y

euler92 = show . length . flip evalState initial . filterM valid $ [1..10000000]
  where
    initial = IM.fromAscList [(1,False),(89,True)]
    valid :: Int -> State (IM.IntMap Bool) Bool
    valid x = do mv <- gets $ IM.lookup x
                 case mv of
                   (Just b) -> return b
                   _ -> do delegate <- valid (sum . map (^2) . digits $ x)
                           modify $ IM.insert x delegate
                           return delegate

euler93 = fst . maximumBy (comparing snd) . map (disp &&& ct) $ groups
  where
    disp = concatMap show . map numerator
    groups = rCombinations 4 ([1..9] :: [Rational])
    liftOp f a b = return $ f a b
    ops = [div,flip div] ++ map liftOp [(+),(*),(-),flip (-)]
    -- we use the flipped versions of (-) and div /instead of/ implementing
    -- precedence. Therefore it's not redundant with taking the permutations of
    -- a,b,c,d. The permutation selected sets the evaulation order, and the
    -- flipped versions allow one or the other operation.
    div _ 0 = mzero
    div x y = return $ x / y
    vals [a,b,c,d] = do [w,x,y,z] <- permutations [a,b,c,d]
                        f <- ops
                        g <- ops
                        h <- ops
                        ret <- f w =<< g x =<< h y z
                        guard $ denominator ret == 1 && ret > 0
                        return $ numerator ret
    ct = length . takeWhile id . zipWith (==) [1..] . Sorted.nub . sort . vals
