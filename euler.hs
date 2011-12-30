module Main where

import Set0 (set0)
import Set1 (set1)
import Set2 (set2)
import Set3 (set3)
import Set4 (set4)
import Set5 (set5)
import Set6 (set6)
import Set7 (set7)

lastProblem = 75
sets = [set0,set1,set2,set3,set4,set5,set6,set7]

driver input = (sets !! s) !! p
    where
      (s,p) = (read input :: Int) `quotRem` 10

main = do tmp <- getLine
          if tmp == "q"
          then return ()
          else do if tmp == "check"
                  then mapM_ (putStrLn . show) . zip [1..lastProblem] . tail . concat $ sets
                  else putStrLn ('>' : driver tmp)
                  main
