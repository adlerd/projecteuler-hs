module Monopoly (euler84) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Data.Array
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Ratio

euler84 = formAnswer . (!! 50)
          . iterate (tableNormalize stateBounds . (fullTurn =<<)) . return
          $ theState 0 0

newtype Chances a = Ch { unCh :: [(a, Rational)] }

instance (NFData a => NFData (Chances a)) where
    rnf (Ch x) = rnf x

instance (Monad Chances) where
  return = Ch . (:[]) . flip (,) 1
  m >>= f = Ch [(y, xp*yp) | (x, xp) <- unCh m, (y, yp) <- unCh $ f x]

instance (Functor Chances) where
  fmap f = Ch . map (first f) . unCh

equally :: [a] -> Chances a
equally xs = Ch . map (flip (,) p) $ xs
    where p = 1 % (fromIntegral $ length xs)

tableNormalize :: (Ix a) => (a, a) -> Chances a -> Chances a
tableNormalize _ (Ch []) = Ch []
tableNormalize bnds ch = Ch . filter ((/= (0 % 1)) . snd) . assocs
                         . accumArray (+) 0 bnds . unCh $ ch

normalizeBy :: (a -> a -> Ordering) -> Chances a -> Chances a
normalizeBy _ (Ch []) = Ch []
normalizeBy ord ch = Ch regrouped
    where
      regrouped = map norm . groupBy isEq . sortBy fstOrd . unCh $ ch
      norm = (head *** sum) . unzip
      isEq a b = (fstOrd a b) == EQ
      fstOrd a b = fst a `ord` fst b

newtype Square = Square { unSquare :: Int }
  deriving (Ix, Ord, Eq, Show)

instance (NFData Square) where
    rnf (Square s) = rnf s

newtype Doubles = Doubles { unDoubles :: Int }
  deriving (Ix, Ord, Eq, Show)

instance (NFData Doubles) where
    rnf (Doubles d) = rnf d

type State = (Square, Doubles)

stateBounds :: (State, State)
stateBounds = (minBound, maxBound)

instance (Bounded Square) where
  minBound = Square 0
  maxBound = Square 39

instance (Bounded Doubles) where
  minBound = Doubles 0
  maxBound = Doubles 2

theState s d = (Square s, Doubles d)

roll :: State -> Chances State
roll (Square sq, Doubles db) = normalizeBy compare $ do
  d1 <- rollDie
  d2 <- rollDie
  let newd = if d1 == d2 then db + 1 else 0
  return $ if newd == 3
           then theState 10 0
           else theState ((sq + d1 + d2) `rem` unSquare maxBound) newd
    where
      rollDie = equally [1..4]

jail :: State
jail = theState 10 0

g2gFire :: State -> State
g2gFire (Square 30, _) = jail
g2gFire st = st

type Card = State -> State

chanceDeck,chestDeck :: Chances Card
chanceDeck = Ch . map (second (% 16)) $ nothings ++ gos ++ goJail ++ finds
    where
      nothings = [(id, 6)]
      gos = map (single . goSq) [0, 11, 24, 39, 5]
      goJail = [single $ const jail]
      finds = (first nextR, 2) : map (single . first) [nextU, back3]
      single = flip (,) 1
chestDeck = Ch . map (second (% 16)) $ [(goSq 0,1), (const jail,1), (id, 14)]

circleSquare :: Int -> Square
circleSquare = Square . (`mod` (unSquare maxBound + 1))

goSq :: Int -> Card
goSq = first . const . Square

nextR,nextU,back3 :: Square -> Square
nextR = circleSquare . (+ 5) . (* 10) .  (`quot` 10)
        . (+ 5) . unSquare
nextU (Square s)
  | s < 28 && s > 11 = Square 28
  | otherwise = Square 12
back3 = circleSquare . (+ 37) . unSquare

drawCard :: Chances Card -> [Square] ->  State -> Chances State
drawCard deck when st
  | (`elem` when) . fst $ st = deck >>= (return . ($ st))
  | otherwise = return st

chanceSquares = map Square [36, 22, 7]
chestSquares = map Square [33, 17, 2]

fullTurn' = drawCard chestDeck chestSquares <=< drawCard chanceDeck chanceSquares
            <=< return . g2gFire <=< roll

fullTurnArr = listArray stateBounds
              . map (tableNormalize stateBounds . fullTurn')
              $ range stateBounds

fullTurn = fullTurnArr `deepseq` (fullTurnArr !)

formAnswer :: Chances State -> String
formAnswer = concatMap (tail . show . (+ 100) . fst) . take 3
             . sortBy (comparing $ negate . snd) . unCh . normalizeBy compare
             . fmap (unSquare . fst)
