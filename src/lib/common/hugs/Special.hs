{-*-----------------------------------------------------------------------
  The Core Assembler.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

{---------------------------------------------------------------
  Special definitions for the Hugs system
---------------------------------------------------------------}
module Special( doesFileExist
              , openBinary, closeBinary, readBinary, writeBinaryChar
              , ST, STArray, runST, newSTArray, readSTArray, writeSTArray
              ) where

import IO           ( IOMode(..), openFile, ReadMode, hClose )
import IOExtensions ( readBinaryFile, writeBinaryFile )
import IOExts       ( IORef, newIORef, writeIORef, readIORef )
import LazyST       ( ST, STArray, runST, newSTArray, readSTArray, writeSTArray )

doesFileExist fname
  = do{ h <- openFile fname ReadMode
      ; hClose h
      ; return True
      }
    `catch` \err -> return False


data BHandle  = BRead FilePath
              | BWrite FilePath (IORef String)

openBinary :: FilePath -> IOMode -> IO BHandle
openBinary path mode
  = case mode of
      ReadMode  -> return (BRead path)
      WriteMode -> do{ r <- newIORef ""
                     ; return (BWrite path r)
                     }
      other     -> error "Special.openBinary: invalid open mode"

closeBinary :: BHandle -> IO ()
closeBinary (BRead path)
  = return ()

closeBinary (BWrite path r)
  = do{ xs <- readIORef r
      ; writeBinaryFile path (reverse xs)
      }

readBinary :: BHandle -> IO String
readBinary (BRead path)
  = readBinaryFile path

writeBinaryChar :: BHandle -> Char -> IO ()
writeBinaryChar (BWrite path r) c
  = do{ xs <- readIORef r
      ; writeIORef r (c:xs)
      ; return ()
      }
