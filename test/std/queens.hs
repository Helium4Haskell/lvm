----------------------------------------------------------------
-- Daan Leijen (c) 2001
-- 
-- $Revision$
-- $Author$
-- $Date$
----------------------------------------------------------------
module Main where

import Win32 (timeGetTime)

tickCount :: IO Int
tickCount = do{ t <- timeGetTime
              ; return (fromIntegral t)
              }

main  = do{ t1 <- tickCount
          ; let n = length (queens 9 9)
          ; t2 <- seq n tickCount
          ; putStr $ "running time: " ++ show (t2 - t1) ++ " msecs.\n"
          }

----------------------------------------------------------------
--
----------------------------------------------------------------
queens k 0  = [[]]
--queens k n  = [ (x:xs) | xs <- queens k (n-1), x <- [1..k], safe x 1 xs ]
queens k n  = let xss = queens k (n-1)
                  walk []       = []
                  walk (xs:xss) = let walkx 0  = walk xss
                                      walkx x  | safe x 1 xs  = (x:xs):walkx (x-1)
                                               | otherwise    = walkx (x-1)
                                  in  walkx k
              in  walk xss

safe x d []     = True
safe x d (y:ys) = x /= y && x+d /= y && x-d /= y && safe x (d+1) ys
            