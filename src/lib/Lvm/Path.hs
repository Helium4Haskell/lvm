{------------------------------------------------------------------------
  The Core Assembler.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

module Lvm.Path
   ( getLvmPath, searchPath, searchPathMaybe, splitPath
   ) where

import Data.List
import System.Directory
import System.Environment

----------------------------------------------------------------
-- file searching
----------------------------------------------------------------

searchPath :: [String] -> String -> String -> IO String
searchPath path ext name = 
    fmap (maybe (fail ("could not find " ++ show nameext)) id)
         (searchPathMaybe path ext name)
  where
    nameext
      | isPrefixOf (reverse ext) (reverse name)  = name
      | otherwise  = name ++ ext
        
searchPathMaybe :: [String] -> String -> String -> IO (Maybe String)
searchPathMaybe  path ext name
  = walk (map makeFName path) -- was ("":path), but we don't want to look in the current directory by default
  where
    walk []         = return Nothing
    walk (fname:xs) = do{ exist <- doesFileExist fname
                        ; if exist
                           then return (Just fname)
                           else walk xs
                        }

    makeFName dir
      | null dir          = nameext
      | last dir == '/' ||
        last dir == '\\'  = dir ++ nameext
      | otherwise         = dir ++ "/" ++ nameext

    nameext
      | isPrefixOf (reverse ext) (reverse name)  = name
      | otherwise  = name ++ ext

getLvmPath :: IO [String]
getLvmPath
  = do{ xs <- getEnv "LVMPATH"
      ; return (splitPath xs)
      }
  `catch` \_ -> return []

splitPath :: String -> [String]
splitPath = walk [] ""
  where
    walk ps p xs
      = case xs of
          []             -> if (null p)
                             then reverse ps
                             else reverse (reverse p:ps)
          (';':cs)       -> walk (reverse p:ps) "" cs
          (':':'\\':cs)  -> walk ps ("\\:" ++ p) cs
          (':':cs)       -> walk (reverse p:ps) "" cs
          (c:cs)         -> walk ps (c:p) cs