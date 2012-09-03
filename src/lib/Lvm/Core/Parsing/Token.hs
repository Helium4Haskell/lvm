{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: Lexer.hs 269 2012-08-31 15:16:49Z bastiaan $

module Lvm.Core.Parsing.Token 
   ( Token, Lexeme(..), Pos, incpos, newpos
   ) where

import Text.PrettyPrint.Leijen (Pretty(..))

-----------------------------------------------------------
-- Tokens and lexems
-----------------------------------------------------------

type Pos        = (Int,Int)
type Token      = (Pos,Lexeme)

data Lexeme     = LexUnknown Char
                | LexError String
                | LexChar Char
                | LexString String
                | LexInt Integer
                | LexFloat Double
                | LexId String
                | LexQualId String String
                | LexOp String
                | LexCon String
                | LexQualCon String String
                | LexConOp String

                | LexCOMMA      -- ,
                | LexQUOTE      -- `
                | LexSEMI       -- ;
                | LexBSLASH     -- \ (niet meteen een enter hierachter vanwege -cpp)
                | LexASG        -- =
                | LexCOLON      -- :
                | LexCOLCOL     -- ::
                | LexDOT        -- .
                | LexDOTDOT     -- ..
                | LexBAR        -- |
                | LexLARROW     -- <-
                | LexRARROW     -- ->
                | LexTILDE      -- ~
                | LexARROW      -- =>
                | LexAT         -- @
                | LexEXCL       -- !
                | LexDASH       -- -

                | LexLPAREN     -- (
                | LexRPAREN     -- )
                | LexLBRACKET   -- [
                | LexRBRACKET   -- ]
                | LexLBRACE     -- {
                | LexRBRACE     -- }

                | LexLET
                | LexIN
                | LexDO
                | LexWHERE
                | LexCASE
                | LexOF
                | LexIF
                | LexTHEN
                | LexELSE
                | LexDATA
                | LexTYPE
                | LexMODULE
                | LexIMPORT
                | LexEOF

                -- not standard
                | LexLETSTRICT
                | LexMATCH
                | LexWITH

                | LexPRIVATE
                | LexPUBLIC
                | LexDEFAULT
                | LexCON
                
                | LexABSTRACT
                | LexINSTR
                | LexEXTERN

                | LexNOTHING
                | LexCUSTOM

                | LexSTATIC | LexDYNAMIC | LexRUNTIME
                | LexCCALL | LexSTDCALL | LexINSTRCALL
                | LexDECORATE | LexORDINAL
      deriving (Eq,Show)

instance Pretty Lexeme where
   pretty = pretty . show

-----------------------------------------------------------
-- Positions
-----------------------------------------------------------

incpos :: Pos -> Int -> Pos
incpos (line,col) i     = (line,col+i)

newpos :: Pos -> Char -> Pos
newpos (line,_)   '\n'  = (line + 1,1)
newpos (line,col) '\t'  = (line, ((((col-1) `div` 8)+1)*8)+1)
newpos (line,col) _     = (line, col+1)
