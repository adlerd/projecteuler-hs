module Main where

import Set0
import Set1
import Set2
import Set3
import Set4
import Set5
import Set6
import Set7
import Set8
import Set9

import Data.Maybe (fromJust)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Control.Exception as CE
import Control.Monad (guard)
import System.IO.Error (isEOFError)
import Control.Arrow ((&&&))
import System.IO
import Control.Parallel.Strategies

sets :: [[(Int, String)]]
sets = [set0,set1,set2,set3,set4,set5,set6,set7,set8,set9]

parSets = map lookupProblem [41,44,50,51,73,75,78,87]

sets' = snd . withStrategy s $ (parSets, concat sets)
  where
    s = parTuple2 (parList rdeepseq) r0

lookupProblem n = fromJust . lookup n . (!! (n `quot` 10)) $ sets

getCommand = CE.catchJust (guard . isEOFError) getLine (\_ -> return "q")


main = do hSetBuffering stdout LineBuffering
          loop

loop = do command <- getCommand
          case command of
           "q"     -> return ()
           "check" -> mapM_ (hPrint stdout) . sortBy (comparing fst) $ sets'
           _       -> (>> loop) . print . (id &&& lookupProblem)
                      . (read :: String -> Int) $ command
