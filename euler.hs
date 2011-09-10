module Main where

import Set0
import Set1
import Set2
import Set3
import Set4

sets = [set0, set1,set2,set3,set4]

driver input = (sets !! s) !! p
    where
      (s,p) = (read input :: Int) `quotRem` 10      

main = do tmp <- getLine
          if tmp == "q"
          then return ()
          else do putStrLn ('>' : driver tmp)
                  main          

--main = mapM_ (putStrLn . show) . zip [1..] . tail . concat $ sets
