module Main where 


import Win32 (timeGetTime)

tickCount :: IO Int
tickCount = do{ t <- timeGetTime
              ; return (fromIntegral t)
              }

main  = do{ t1 <- tickCount
          ; let n = last (take 1000 primes)
          ; t2 <- seq n tickCount
          ; putStr $ "running time: " ++ show (t2 - t1) ++ " msecs.\n"
          }


-- main    = print (last (take 1000 primes))

primes  = sieve [3,5..]
        where
          sieve (x:xs)  = x:sieve (filter (noDiv x) xs)
          noDiv x y     = (y `mod` x /= 0)
