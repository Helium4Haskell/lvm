module Main where 

import Win32Misc (timeGetTime)

tickCount :: IO Int
tickCount = do{ t <- timeGetTime
              ; return (fromIntegral t)
              }

main  = do{ t1 <- tickCount
          ; let n = nfib 27
          ; t2 <- seq n tickCount
          ; putStr $ "running time: " ++ show (t2 - t1) ++ " msecs.\n"
          ; putStr $ "nfib number : " ++ show ((n * 1000) `div` (t2 - t1)) ++ "\n"
          }

-- normal nfib
nfib :: Int -> Int
nfib 0  = 1
nfib 1  = 1
nfib n  = 1 + nfib (n-1) + nfib (n-2)

{-
fib n  = case n of
            0 -> 1
            1 -> 1
            default -> add (fib (sub n 1)) (fib (sub n 2))

=> case on n => strict
fib n!  = case n! of
            0 -> 1
            1 -> 1
            default -> add (fib (sub n! 1)) (fib (sub n! 2))

=> strict add
fib n!  = case n! of
            0 -> 1
            1 -> 1
            default -> case (fib (sub n! 1)) of x! -> 
                       case (fib (sub n! 2)) of y! ->
                       add x! y!
=> strict fib
fib n!  = case n! of
            0 -> 1
            1 -> 1
            default -> case (sub n! 1) of a! ->
                       case (fib a!) of x! -> 
                       case (sub n! 2) of b! ->
                       case (fib b!) of y! ->
                       add x! y!

=> inline add & case x! of x! -> e == e
fib n!  = case n! of
            0 -> 1
            1 -> 1
            default -> case (sub n! 1) of a! ->
                       case (fib a!) of x! -> 
                       case (sub n! 2) of b! ->
                       case (fib b!) of y! ->
                       add! x! y!
=> same for sub
fib n!  = case n! of
            0 -> 1
            1 -> 1
            default -> case! (sub! n! 1) of a! ->
                       case (fib a!) of x! -> 
                       case! (sub! n! 2) of b! ->
                       case (fib b!) of y! ->
                       add! x! y!


-}
