{------------------------------------------------------------------------
  The Core Assembler.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

{---------------------------------------------------------------
  Special definitions for the GHC system
---------------------------------------------------------------}
module Special( doesFileExist
              , openBinary, closeBinary, readBinary, writeBinaryChar
              , ST, STArray, runST, newSTArray, readSTArray, writeSTArray
              , unsafeCoerce
              ) where

import Directory  ( doesFileExist )
import IO         ( Handle, hGetContents, hClose, hPutChar, IOMode(..) )
#if (__GLASGOW_HASKELL__ >= 602)
import GHC.Handle ( openBinaryFile )
#else
import GHC.Handle ( openFileEx, IOModeEx(..))
#endif

#if (__GLASGOW_HASKELL__ >= 503)
import GHC.Base         ( unsafeCoerce# )
import GHC.Arr          ( STArray, newSTArray, readSTArray, writeSTArray)
import Control.Monad.ST ( runST, ST ) 
#else
import GlaExts  ( unsafeCoerce# )
import LazyST   ( ST, STArray, runST, newSTArray, readSTArray, writeSTArray)
#endif
              
unsafeCoerce :: a -> b
unsafeCoerce x
  = unsafeCoerce# x

openBinary :: FilePath -> IOMode -> IO Handle
#if (__GLASGOW_HASKELL__ >= 602)
openBinary
  = openBinaryFile
#else
openBinary path mode
  = openFileEx path (BinaryMode mode)
#endif

closeBinary :: Handle -> IO ()
closeBinary h
  = hClose h

readBinary :: Handle -> IO String
readBinary h
  = do{ xs <- hGetContents h
      ; seq (last xs) $ hClose h
      ; return xs
      }

writeBinaryChar :: Handle -> Char -> IO ()
writeBinaryChar h c
  = hPutChar h c
