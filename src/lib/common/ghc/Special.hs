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
  Special definitions for the GHC system
---------------------------------------------------------------}
module Special( doesFileExist
              , openBinary, closeBinary, readBinary, writeBinaryChar
              ) where

import Directory( doesFileExist )
import IO       ( Handle, hGetContents, hClose, hPutChar, IOMode(..) )
import IOExts   ( openFileEx, IOModeEx(..) )

openBinary :: FilePath -> IOMode -> IO Handle
openBinary path mode
  = openFileEx path (BinaryMode mode)

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
