module Main where

import Char   (isSpace)
import System (exitWith,getArgs,ExitCode(..))

main  
  = do{ [fname1,fname2] <- getArgs
      ; xs <- readFile fname1
      ; ys <- readFile fname2
      ; if (clean xs /= clean ys)
         then do{ putStrLn "different files"
                ; exitWith (ExitFailure 1)
                }
         else return ()
      }
  where
    clean xs  = filter (not . isSpace) xs
   
