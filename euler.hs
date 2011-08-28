module Main where

import Set0

sets = [set0]

driver input = (sets !! s) !! p
    where
      (s,p) = (read input :: Int) `quotRem` 10      

main = do tmp <- getLine
          putStrLn ('>' : driver tmp)
          main
