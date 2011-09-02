module Main where

import Set0
import Set1

sets = [set0, set1]

driver input = (sets !! s) !! p
    where
      (s,p) = (read input :: Int) `quotRem` 10      

main = do tmp <- getLine
          putStrLn ('>' : driver tmp)
          main

--main = mapM_ (putStrLn . show) . zip [1..] . tail . concat $ sets
