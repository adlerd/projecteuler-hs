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

import Data.Maybe (fromJust)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Control.Exception as CE
import Control.Monad (guard)
import System.IO.Error (isEOFError)
import Control.Arrow ((&&&))

sets :: [[(Int, String)]]
sets = [set0,set1,set2,set3,set4,set5,set6,set7,set8]

lookupProblem n = fromJust . lookup n . (!! (n `quot` 10)) $ sets

getCommand = CE.catchJust (guard . isEOFError) getLine (\_ -> return "q")

main = do command <- getCommand
          case command of
           "q"     -> return ()
           "check" -> mapM_ (putStrLn . show) . sortBy (comparing fst) . concat
                      $ sets
           _       -> (>> main) . print . (id &&& lookupProblem)
                      . (read :: String -> Int) $ command
