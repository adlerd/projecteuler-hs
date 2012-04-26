module PQ
(Queue(..), findMin, insert, deleteMin, dumpQueue)
 where

import Data.List (minimumBy,partition,unfoldr)
import Data.Ord (comparing)

data Node a = Node { elt::a, rank::Int, children::[Node a] }
              deriving Show
type Forest a = [Node a]
data Queue a = Empty | Queue { root::a, forest::(Forest a) }
               deriving Show

findMin :: (Ord a) => Queue a -> a
findMin = root

meldForest :: (Ord a) => Forest a -> Forest a -> Forest a
meldForest a b = meldForest' (u a) (u b)
    where
      u [] = []
      u (x:xs) = insertForestPNode x xs
      meldForest' [] x = x
      meldForest' x [] = x
      meldForest' aa@(a:as) bb@(b:bs) =
          case comparing rank a b of
            LT -> a : meldForest' as bb
            GT -> b : meldForest' aa bs
            EQ -> insertForestPNode (link a b) $ meldForest' as bs

insertForestPNode :: (Ord a) => Node a -> Forest a -> Forest a
insertForestPNode n [] = [n]
insertForestPNode n@(Node _ r0 _) all@((n1@(Node _ r1 _)):ns)
    | r0 < r1 = n:all
    | r0 == r1 = insertForestPNode (link n n1) ns
    | otherwise = error $ show (r0,r1)

{-
link :: (Ord a) => Node a -> Node a -> Node a
link n1@(Node e1 r c1) n2@(Node e2 _ c2)
    | e1 < e2 = Node e1 (r + 1) $ n2:c1
    | otherwise = Node e2 (r + 1) $ n1:c2 -}
link :: (Ord a) => Node a -> Node a -> Node a
link n1@(Node e1 r1 c1) n2@(Node e2 r2 c2)
     | r1 /= r2 = undefined
     | e1 < e2 = Node e1 (r1 + 1) $ n2:c1
     | otherwise = Node e2 (r2 + 1) $ n1:c2

insertForest0Node :: (Ord a) => Node a -> Forest a -> Forest a
insertForest0Node n all@(n1:n2:rest)
    | r1 == r2 = skewLink : rest
    | otherwise = n : all
    where
      e0 = elt n
      (Node e1 r1 c1) = n1
      (Node e2 r2 c2) = n2
      r = 1 + r1
      skewLink | e0 < e1 = if e0 < e2
                           -- not (e1 <= e0 || e2 <= e0)
                           then (Node e0 r [n1,n2])
                           -- e2 <= e0 < e1
                           else (Node e2 r $ n:n1:c2)
               | e1 <= e2 = (Node e1 r $ n:n2:c1) -- e1 <= e0 && e1 <= e2
               | otherwise = (Node e2 r $ n:n1:c2) -- e2 < e1 <= e0
insertForest0Node n all = n : all

remMinBy :: (a -> a -> Ordering) -> [a] -> (a, [a])
remMinBy cmp xx = rmb xx
    where
      rmb [x] = (x, [])
      rmb (x:xs) = case cmp x x' of
                     LT -> (x,xs)
                     otherwise -> (x',x:xs')
          where
            (x',xs') = rmb xs

deleteMin :: (Ord a) => Queue a -> Queue a
deleteMin (Queue _ []) = Empty
deleteMin (Queue _ f) = Queue next nextForest
    where
      ((Node next _ rest), restForest) = remMinBy (comparing elt) f
      (rank0s, rankPs) = revPart [] [] rest
          where
            revPart r0s rPs [] = (r0s,rPs)
            revPart r0s rPs (c@(Node _ r _):cs)
                | r == 0 = revPart (c:r0s) rPs cs
                | otherwise = revPart r0s (c:rPs) cs
      nextForest = foldr insertForest0Node melded rank0s
      melded = meldForest rankPs restForest

insert :: (Ord a) => a -> Queue a -> Queue a
insert x Empty = Queue x []
insert x (Queue r f) = Queue min $ insertForest0Node (Node max 0 []) f
    where
      (min, max) = if x < r then (x, r) else (r, x)

dumpQueue :: (Ord a) => Queue a -> [a]
dumpQueue = unfoldr dq
    where
      dq Empty = Nothing
      dq q@(Queue r _) = Just (r, deleteMin q)
