{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

module Lvm.Core.Parse
    ( coreParse, coreParseExport, coreParseExpr, coreParseType
    ) where

import Prelude hiding (lex)
import Text.ParserCombinators.Parsec hiding (satisfy)
import Lvm.Common.Byte   ( Bytes, bytesFromString )
import Lvm.Common.IdSet
import Lvm.Core.Data
import Lvm.Core.Utils
import Lvm.Core.Lexer
import Lvm.Core.Type
import Data.List (foldl') 

----------------------------------------------------------------
-- Parse a Core source file
----------------------------------------------------------------
coreParse :: FilePath -> IO CoreModule
coreParse = coreParseModule parseModule

coreParseExport :: FilePath -> IO (CoreModule, Bool, (IdSet,IdSet,IdSet,IdSet,IdSet))
coreParseExport = coreParseModule parseModuleExport

coreParseModule :: TokenParser a -> FilePath -> IO a
coreParseModule parser fname =
    do{ input  <- readFile fname
      ; res <- case runParser parser () fname (layout (lexer (1,1) input)) of
          Left err
            -> ioError (userError ("parse error: " ++ show err))
          Right res
            -> return res
      --; (putStrLn.show) res
      ; return res
      }

coreParseAny :: TokenParser a -> String -> String -> a
coreParseAny parser fname input =
    case runParser parser () fname (lexer (1,1) input) of {
        Left  err -> error ("\n" ++ fname ++ ": " ++ show err);
        Right res -> res;
    }

coreParseExpr :: String -> String -> Expr
coreParseExpr fname input = coreParseAny pexpr fname input

coreParseType :: String -> String -> Type
coreParseType fname input = addForall (coreParseAny ptypeFun fname input)

----------------------------------------------------------------
-- Basic parsers
----------------------------------------------------------------
type TokenParser a  = GenParser Token () a   

----------------------------------------------------------------
-- Program
----------------------------------------------------------------

wrap :: TokenParser a -> TokenParser [a]
wrap p
  = do{ x <- p; return [x] }

parseModule :: TokenParser CoreModule
parseModule =
    do{ (m, _, _) <- parseModuleExport
      ; return m
      }

parseModuleExport :: TokenParser (CoreModule, Bool, (IdSet,IdSet,IdSet,IdSet,IdSet))
parseModuleExport =
    do{ lexeme LexMODULE
      ; moduleId <- conid <?> "module name"
      ; exports <- pexports
      ; lexeme LexWHERE
      ; lexeme LexLBRACE
      ; declss <- semiList (wrap (ptopDecl <|> pabstract <|> pextern <|> pCustomDecl)
                            <|> pdata <|> pimport <|> ptypeTopDecl)
      ; lexeme LexRBRACE
      ; lexeme LexEOF

      ; return $
            ( case exports of
                Nothing ->
                    let es = (emptySet,emptySet,emptySet,emptySet,emptySet)
                    in
                    ( modulePublic
                        True
                        es
                        (Module moduleId 0 0 (concat declss))
                    , True
                    , es
                    )
                Just es ->
                    ( modulePublic
                        False
                        es
                        (Module moduleId 0 0 (concat declss))
                    , False
                    , es
                    )
            )
      }

----------------------------------------------------------------
-- export list
----------------------------------------------------------------
data Export  = ExportValue Id
             | ExportCon   Id
             | ExportData  Id 
             | ExportDataCon Id
             | ExportModule Id

pexports :: TokenParser (Maybe (IdSet,IdSet,IdSet,IdSet,IdSet))
pexports
  = do{ exports <- commaParens pexport <|> return []
      ; return $
            if null (concat exports) then
                Nothing
            else
                Just (foldl'
                    split
                    (emptySet,emptySet,emptySet,emptySet,emptySet)
                    (concat exports)
                )
      }
  where
    split (values,cons,datas,datacons,ms) export
      = case export of
          ExportValue   x -> (insertSet x values,cons,datas,datacons,ms)
          ExportCon     x -> (values,insertSet x cons,datas,datacons,ms)
          ExportData    x -> (values,cons,insertSet x datas,datacons,ms)
          ExportDataCon x -> (values,cons,datas,insertSet x datacons,ms)
          ExportModule  x -> (values,cons,datas,datacons,insertSet x ms)

pexport :: TokenParser [Export]
pexport
  = do{ lexeme LexLPAREN
      ; entity <-
            do { x <- opid   ; return (ExportValue x) }
            <|>
            do { x <- conopid; return (ExportCon   x) }
      ; lexeme LexRPAREN
      ; return [entity]
      }
  <|>
    do{ x <- varid
      ; return [ExportValue x]
      }
  <|>
    do{ x <- typeid
      ; do{ lexeme LexLPAREN
          ; cons <- pexportCons x
          ; lexeme LexRPAREN
          ; return (ExportData x:cons)
          }
        <|>
        -- no parenthesis: could be either a
        -- constructor or a type constructor
        return [ExportData x, ExportCon x]
      }      
  <|>
    do{ lexeme LexMODULE
      ; x <- conid
      ; return [ExportModule x]
      }

pexportCons :: Id -> TokenParser [Export]
pexportCons x
  = do{ lexeme LexDOTDOT
      ; return [ExportDataCon x]
      }
  <|>
    do{ xs <- sepBy constructor (lexeme LexCOMMA)
      ; return (map ExportCon xs)
      }


----------------------------------------------------------------
-- abstract declarations
----------------------------------------------------------------
pabstract :: TokenParser CoreDecl
pabstract
  = do{ lexeme LexABSTRACT
      ; pabstractValue <|> pabstractCon
      }

pabstractValue :: TokenParser (Decl v)
pabstractValue
  = do{ x <- variable
      ; lexeme LexASG
      ; (mid,impid) <- qualifiedVar
      ; (_,tparity) <- ptypeDecl
      ; arity <- do{ lexeme LexASG; i <- lexInt; return (fromInteger i) } <|> return tparity
      ; return (DeclAbstract x (Imported False mid impid DeclKindValue 0 0) arity [])
      }

pabstractCon :: TokenParser (Decl v)
pabstractCon
  = do{ x <- conid
      ; lexeme LexASG
      ; (mid,impid) <- qualifiedCon
      ; (_,arity) <- ptypeDecl
      ; lexeme LexASG
      ; tag <- lexInt
      ; return (DeclCon x (Imported False mid impid DeclKindCon 0 0) arity (fromInteger tag) [])
      }


----------------------------------------------------------------
-- import declarations
----------------------------------------------------------------
pimport :: TokenParser [CoreDecl]
pimport
  = do{ lexeme LexIMPORT
      ; mid <- conid
      ; do{ xss <- commaParens (pImportSpec mid)
          ; return (concat xss)
          }
        <|>
        return [DeclImport mid (Imported False mid dummyId DeclKindModule 0 0) []]
      }

pImportSpec :: Id -> TokenParser [CoreDecl]
pImportSpec mid
  = do{ lexeme LexLPAREN
      ; (kind, x) <-
            do { y <- opid   ; return (DeclKindValue, y) }
            <|>
            do { y <- conopid; return (DeclKindCon  , y) }
      ; lexeme LexRPAREN
      ; impid <- option x (do{ lexeme LexASG; variable })
      ; return [DeclImport x (Imported False mid impid kind 0 0) []]
      }
  <|>
    do{ x <- varid
      ; impid <- option x (do{ lexeme LexASG; variable })
      ; return [DeclImport x (Imported False mid impid DeclKindValue 0 0) []]
      }
  <|>
    do{ lexeme LexCUSTOM
      ; kind <- lexString
      ; x   <- variable <|> constructor
      ; impid <- option x (do { lexeme LexASG; variable <|> constructor })
      ; return [DeclImport x (Imported False mid impid (customDeclKind kind) 0 0) []]
      }
  <|>
    do{ x <- typeid
      ; impid <- option x (do{ lexeme LexASG; variable })
      ; do{ lexeme LexLPAREN
          ; cons <- pImportCons mid
          ; lexeme LexRPAREN
          ; return (DeclImport x (Imported False mid impid customData 0 0) [] : cons)
          }
        <|>
        return
            [DeclImport x (Imported False mid impid DeclKindCon 0 0) []]
      }

pImportCons :: Id -> TokenParser [CoreDecl]
pImportCons mid
  = -- do{ lexeme LexDOTDOT
    --   ; return [ExportDataCon id]
    --   }
  -- <|>
    sepBy (pimportCon mid) (lexeme LexCOMMA)

pimportCon :: Id -> TokenParser CoreDecl
pimportCon mid
  = do{ x    <- constructor
      ; impid <- option x (do{ lexeme LexASG; variable })
      ; return (DeclImport x (Imported False mid impid DeclKindCon 0 0) [])
      }

----------------------------------------------------------------
-- value declarations
----------------------------------------------------------------
ptopDecl :: TokenParser CoreDecl
ptopDecl
  = do{ x <- variable
      ; ptopDeclType x <|> ptopDeclDirect x
      }

ptopDeclType :: Id -> TokenParser (Decl Expr)
ptopDeclType x
  = do{ (tp,_) <- ptypeDecl
      ; lexeme LexSEMI
      ; x2  <- variable
      ; if (x /= x2)
         then fail
            (  "identifier for type signature "
            ++ stringFromId x
            ++ " doesn't match the definition"
            ++ stringFromId x2
            )
         else return ()
      ; (access,custom,expr) <- pbindTopRhs
      ; return (DeclValue x access Nothing expr 
                ([customType tp] ++ custom))
      }

ptopDeclDirect :: Id -> TokenParser (Decl Expr)
ptopDeclDirect x
  = do{ (access,custom,expr) <- pbindTopRhs
      ; return (DeclValue x access Nothing expr custom)
      }

pbindTopRhs :: TokenParser (Access, [Custom], Expr)
pbindTopRhs
  = do{ args <- many bindid
      ; (access,custom) <- pAttributes
      ; lexeme LexASG
      ; body <- pexpr
      ; let expr = foldr Lam body args
      ; return (access,custom,expr)
      }
  <?> "declaration"



pbind :: TokenParser Bind
pbind
  = do{ x   <- variable
      ; expr <- pbindRhs
      ; return (Bind x expr)
      }

pbindRhs :: TokenParser Expr
pbindRhs
  = do{ args <- many bindid
      ; lexeme LexASG
      ; body <- pexpr
      ; let expr = foldr Lam body args
      ; return expr
      }
  <?> "declaration"

----------------------------------------------------------------
-- data declarations
----------------------------------------------------------------

makeCustomBytes :: String -> Bytes -> Custom
makeCustomBytes k bs = CustomDecl (customDeclKind k) [CustomBytes bs]

customType :: Type -> Custom
customType = makeCustomBytes "type" . bytesFromString . show

customKind :: Kind -> Custom
customKind = makeCustomBytes "kind" . bytesFromString . show

pdata :: TokenParser [CoreDecl]
pdata
  = do{ lexeme LexDATA
      ; x   <- typeid
      ; args <- many typevarid
      ; let kind     = foldr KFun KStar (map (const KStar) args)
            datadecl = DeclCustom x private customData [customKind kind]
      ; do{ lexeme LexASG
          ; let t1  = foldl TAp (TCon x) (map TVar args)
          ; cons <- sepBy1 (pconDecl t1) (lexeme LexBAR)
          ; let con tag (cid,t2) = (DeclCon cid private (arityFromType t2) tag 
                                      [customType t2, 
                                       CustomLink x customData])
          ; return (datadecl:zipWith con [0..] cons)
          }
      <|> {- empty data types -}
        do{ return [datadecl] }
      }

pconDecl :: Type -> TokenParser (Id,Type)
pconDecl tp
  = do{ x   <- constructor
      ; args <- many ptypeAtom
      ; return (x,foldr TFun tp args)
      }

----------------------------------------------------------------
-- type declarations
----------------------------------------------------------------

ptypeTopDecl :: TokenParser [CoreDecl]
ptypeTopDecl
  = do{ lexeme LexTYPE
      ; x   <- typeid
      ; args <- many typevarid
      ; lexeme LexASG
      ; tp   <- ptype
      ; let kind  = foldr KFun KStar (map (const KStar) args)
            tpstr = unwords $  stringFromId x 
                            :  map stringFromId args
                            ++ ["=", show tp]
      ; return [DeclCustom x private customTypeDecl 
                     [CustomBytes (bytesFromString tpstr)
                     ,customKind kind]]
      }

----------------------------------------------------------------
-- Custom
----------------------------------------------------------------
pCustomDecl :: TokenParser CoreDecl
pCustomDecl
  = do{ lexeme LexCUSTOM
      ; kind <- pdeclKind
      ; x   <- customid
      ; (access,customs) <- pAttributes
      ; return (DeclCustom x access kind customs)
      }

pAttributes :: TokenParser (Access,[Custom])
pAttributes
  = do{ lexeme LexCOLON
      ; access  <- paccess
      ; customs <- pcustoms
      ; return (access,customs)
      }
  <|> return (private,[])

paccess :: TokenParser Access
paccess 
  =   do{ lexeme LexPRIVATE; return private }
  <|> do{ lexeme LexPUBLIC; return public }
  <|> return private

pcustoms :: TokenParser [Custom]
pcustoms
  = do{ lexeme LexLBRACKET
      ; customs <- pcustom `sepBy` (lexeme LexCOMMA)
      ; lexeme LexRBRACKET
      ; return customs
      }

pcustom :: TokenParser Custom
pcustom
  =   do{ i <- lexInt; return (CustomInt (fromInteger i)) }
  <|> do{ s <- lexString; return (CustomBytes (bytesFromString s)) }
  <|> do{ x <- variable <|> constructor; return (CustomName x) }
  <|> do{ lexeme LexNOTHING; return (CustomNothing) }
  <|> do{ lexeme LexCUSTOM 
        ; kind <- pdeclKind
        ; do{ x   <- customid
            ; return (CustomLink x kind)
            }
        <|>
          do{ cs   <- pcustoms
            ; return (CustomDecl kind cs)
            }
        }
  <?> "custom value"

pdeclKind :: TokenParser DeclKind
pdeclKind
  =   do{ x <- varid;     return (DeclKindCustom x) }
  <|> do{ i <- lexInt;    return (toEnum (fromInteger i)) }
  <|> do{ s <- lexString; return (customDeclKind s) }
  <?> "custom kind"

----------------------------------------------------------------
-- Expressions
----------------------------------------------------------------

pexpr :: TokenParser Expr
pexpr
  = do{ lexeme LexBSLASH
      ; args <- many bindid
      ; lexeme LexRARROW
      ; expr <- pexpr
      ; return (foldr Lam expr args)
      }
  <|>
    do{ lexeme LexLET
      ; binds <- semiBraces pbind
      ; lexeme LexIN
      ; expr  <- pexpr
      ; return (Let (Rec binds) expr)
      }
  <|>
    do{ lexeme LexCASE
      ; expr <- pexpr
      ; lexeme LexOF
      ; (x,alts) <- palts
      ; case alts of
          [Alt PatDefault rhs] -> return (Let (Strict (Bind x expr)) rhs)
          _                    -> return (Let (Strict (Bind x expr)) (Match x alts))
      }
  <|>
    do{ lexeme LexMATCH
      ; x <- variable
      ; lexeme LexWITH
      ; (defid,alts) <- palts
      ; case alts of
          [Alt PatDefault rhs] -> return (Let (NonRec (Bind defid (Var x))) rhs)
          _                   -> return (Let (NonRec (Bind defid (Var x))) (Match defid alts))
      }
  <|> 
    do{ lexeme LexLETSTRICT
      ; binds <- semiBraces pbind
      ; lexeme LexIN
      ; expr <- pexpr
      ; return (foldr (Let . Strict) expr binds)
      }
  <|> pexprAp
  <?> "expression"

pexprAp :: TokenParser Expr
pexprAp
  = do{ atoms <- many1 patom
      ; return (foldl1 Ap atoms)
      }

patom :: TokenParser Expr
patom
  =   do{ x <- varid; return (Var x)  }
  <|> do{ x <- conid; return (Con (ConId x))  }
  <|> do{ lit <- pliteral; return (Lit lit) }
  <|> parenExpr
  <|> listExpr
  <?> "atomic expression"


listExpr :: TokenParser Expr
listExpr
  = do{ lexeme LexLBRACKET
      ; exprs <- sepBy pexpr (lexeme LexCOMMA)
      ; lexeme LexRBRACKET
      ; return (foldr cons nil exprs)
      }
  where
    cons x xs   = Ap (Ap (Con (ConId (idFromString ":"))) x) xs
    nil         = Con (ConId (idFromString "[]"))

parenExpr :: TokenParser Expr
parenExpr
  = do{ lexeme LexLPAREN
      ; expr <-   do{ x <- opid
                    ; return (Var x)
                    }
                <|>
                  do{ x <- conopid
                    ; return (Con (ConId x))
                    }
                <|> 
                  do{ lexeme LexAT
                    ; tag   <- ptagExpr 
                    ; lexeme LexCOMMA
                    ; arity <- lexInt <?> "arity"
                    ; return (Con (ConTag tag (fromInteger arity)))
                    }
                <|>
                  do{ exprs <- pexpr `sepBy` (lexeme LexCOMMA)
                    ; case exprs of
                        [expr]  -> return expr
                        _       -> let con = Con (ConTag (Lit (LitInt 0)) (length exprs))
                                       tup = foldl Ap con exprs
                                   in return tup
                    }
      ; lexeme LexRPAREN
      ; return expr
      }

ptagExpr :: TokenParser Expr
ptagExpr
  =   do{ i <- lexInt; return (Lit (LitInt (fromInteger i))) }
  <|> do{ x <- variable; return (Var x) }
  <?> "tag (integer or variable)"

pliteral :: TokenParser Literal
pliteral
  =   pnumber id id
  <|> do{ s <- lexString; return (LitBytes (bytesFromString s)) }
  <|> do{ c <- lexChar;   return (LitInt (fromEnum c))   }
  <|> do{ lexeme LexDASH
        ; pnumber negate negate
        }
  <?> "literal"

pnumber :: (Int -> Int) -> (Double -> Double) -> TokenParser Literal
pnumber signint signdouble
  =   do{ i <- lexInt;    return (LitInt (signint (fromInteger i))) }
  <|> do{ d <- lexDouble; return (LitDouble (signdouble d)) }

----------------------------------------------------------------
-- alternatives
----------------------------------------------------------------
palts :: TokenParser (Id,Alts)
palts
  = do{ lexeme LexLBRACE
      ; (x,alts) <- paltSemis
      ; return (x,alts)
      }

paltSemis :: TokenParser (Id,Alts)
paltSemis
  = do{ (x,alt) <- paltDefault
      ; optional (lexeme LexSEMI)
      ; lexeme LexRBRACE
      ; return (x,[alt])
      }
  <|>
    do{ alt <- palt
      ;   do{ lexeme LexSEMI
            ;     do{ (x,alts) <- paltSemis
                    ; return (x,alt:alts)
                    }
              <|> do{ lexeme LexRBRACE
                    ; x <- wildcard
                    ; return (x,[alt])
                    }
            }
      <|> do{ lexeme LexRBRACE
            ; x <- wildcard
            ; return (x,[alt])
            }
      }

palt :: TokenParser Alt
palt  
  = do{ pat <- ppat
      ; lexeme LexRARROW
      ; expr <- pexpr
      ; return (Alt pat expr)
      }

ppat :: TokenParser Pat
ppat  
  = ppatCon <|> ppatLit <|> ppatParens

ppatParens :: TokenParser Pat
ppatParens
  = do{ lexeme LexLPAREN
      ; do{ lexeme LexAT
          ; tag <- lexInt <?> "tag"        
          ; lexeme LexCOMMA
          ; arity <- lexInt <?> "arity"
          ; lexeme LexRPAREN
          ; ids <- many bindid
          ; return (PatCon (ConTag (fromInteger tag) (fromInteger arity)) ids)
          }
        <|>
        do{ x <- conopid
          ; lexeme LexRPAREN
          ; ids <- many bindid
          ; return (PatCon (ConId x) ids)
          }
        <|>
        do{ pat <- ppat <|> ppatTuple 
          ; lexeme LexRPAREN
          ; return pat
          }
      }

ppatCon :: TokenParser Pat
ppatCon
  = do{ x   <- conid <|> do{ lexeme LexLBRACKET; lexeme LexRBRACKET; return (idFromString "[]") }      
      ; args <- many bindid
      ; return (PatCon (ConId x) args)
      }

ppatLit :: TokenParser Pat
ppatLit
  = do{ lit <- pliteral; return (PatLit lit) }

ppatTuple :: TokenParser Pat
ppatTuple
  = do{ ids <- bindid `sepBy` (lexeme LexCOMMA)
      ; return (PatCon (ConTag 0 (length ids)) ids)
      }

paltDefault :: TokenParser (Id, Alt)
paltDefault
  = do{ x <- bindid <|> do{ lexeme LexDEFAULT; wildcard }
      ; lexeme LexRARROW
      ; expr <- pexpr
      ; return (x,Alt PatDefault expr)
      }

wildcard :: TokenParser Id
wildcard
  = identifier (return "_")

----------------------------------------------------------------
-- externs
----------------------------------------------------------------
pextern :: TokenParser CoreDecl
pextern
  = do{ lexeme LexEXTERN
      ; linkConv <- plinkConv
      ; callConv <- pcallConv
      ; x  <- varid
      ; m <- lexString <|> return (stringFromId x)
      ; (mname,name) <- pExternName m
      ; (TString tp,arity)  <- do{ lexeme LexCOLCOL; ptypeString } -- ptypeDecl
      ; return (DeclExtern x private arity tp linkConv callConv mname name [])
      }
  <|>
    do{ lexeme LexINSTR
      ; x <- varid
      ; s  <- lexString
      ; (tp,arity) <- ptypeDecl
      ; return (DeclExtern x private arity (show tp) LinkStatic CallInstr "" (Plain s) [])
      }

------------------

plinkConv :: TokenParser LinkConv
plinkConv
  =   do{ lexeme LexSTATIC; return LinkStatic }
  <|> do{ lexeme LexDYNAMIC; return LinkDynamic }
  <|> do{ lexeme LexRUNTIME; return LinkRuntime }
  <|> return LinkStatic

pcallConv :: TokenParser CallConv
pcallConv
  =   do{ lexeme LexCCALL; return CallC }
  <|> do{ lexeme LexSTDCALL; return CallStd }
  <|> do{ lexeme LexINSTRCALL; return CallInstr }
  <|> return CallC

pExternName :: String -> TokenParser (String, ExternName)
pExternName mname
  =   do{ lexeme LexDECORATE
        ; name <- lexString
        ; return (mname,Decorate name)
        }
  <|> do{ lexeme LexORDINAL
        ; ord  <- lexInt
        ; return (mname,Ordinal (fromIntegral ord))
        }
  <|> do{ name <- lexString
        ; return (mname,Plain name)
        }
  <|> return ("",Plain mname)


----------------------------------------------------------------
-- types
----------------------------------------------------------------

ptypeDecl :: TokenParser (Type, Int)
ptypeDecl
  = do{ lexeme LexCOLCOL
      ; ptypeNormal <|> ptypeString
      }

ptypeNormal :: TokenParser (Type, Int)
ptypeNormal
  = do{ tp <- ptype
      ; return (tp,arityFromType tp)
      }


ptype :: TokenParser Type
ptype
  = ptypeFun

ptypeFun :: TokenParser Type
ptypeFun
  = chainr1 ptypeAp pFun
  where
    pFun  = do{ lexeme LexRARROW; return TFun }

ptypeAp :: TokenParser Type
ptypeAp
  = do{ atoms <- many1 ptypeAtom
      ; return (foldl1 TAp atoms)
      }

ptypeAtom :: TokenParser Type
ptypeAtom
  = do{ x <- typeid
      ; ptypeStrict (TCon x) 
      }
  <|>
    do{ x <- typevarid
      ; ptypeStrict (TVar x)
      }
  <|> listType
  <|> parenType
  <?> "atomic type"

ptypeStrict :: Type -> TokenParser Type
ptypeStrict tp
  = do{ lexeme LexEXCL
      ; return (TStrict tp)
      }
  <|> return tp
      
parenType :: TokenParser Type
parenType
  = do{ lexeme LexLPAREN
      ; tps <- sepBy ptype (lexeme LexCOMMA)
      ; lexeme LexRPAREN
      ; case tps of
          []    -> do{ x <- identifier (return "()"); return (TCon x) } -- (setSortId SortType id))
          [tp]  -> return tp
          _     -> return
                (foldl
                    TAp
                    (TCon (idFromString
                            (  "("
                            ++ replicate (length tps - 1) ','
                            ++ ")"
                            )))
                    tps
                )
      }

listType :: TokenParser Type
listType
  = do{ lexeme LexLBRACKET
      ; do{ tp <- ptype
          ; lexeme LexRBRACKET
          ; x <- identifier (return "[]")
          ; return (TAp (TCon x {- (setSortId SortType id) -}) tp)
          }
      <|>
        do{ lexeme LexRBRACKET
          ; x <- identifier (return "[]")
          ; return (TCon x {-(setSortId SortType id)-})
          }
      }

ptypeString :: TokenParser (Type, Int)
ptypeString
  = do{ s <- lexString
      ; return (TString s, length s-1)
      }

----------------------------------------------------------------
-- helpers
----------------------------------------------------------------

semiBraces, commaParens :: TokenParser a -> TokenParser [a]
semiBraces p  = braces (semiList p)
commaParens p = parens (sepBy p (lexeme LexCOMMA))

braces, parens :: TokenParser a -> TokenParser a
braces p      = between (lexeme LexLBRACE) (lexeme LexRBRACE) p
parens p      = between (lexeme LexLPAREN) (lexeme LexRPAREN) p

-- terminated or seperated
semiList1 :: TokenParser a -> TokenParser [a]
semiList1 p
    = do{ x <- p
        ; do{ lexeme LexSEMI
            ; xs <- semiList p
            ; return (x:xs)
            }
          <|> return [x]
        }

semiList :: TokenParser a -> TokenParser [a]
semiList p
    = semiList1 p <|> return []

----------------------------------------------------------------
-- Lexeme parsers
----------------------------------------------------------------

customid :: TokenParser Id
customid
  =   varid
  <|> conid
  <|> parens (opid <|> conopid)
  <|> do{ s <- lexString; return (idFromString s) }
  <?> "custom identifier"

variable :: TokenParser Id
variable
  = varid <|> parens opid

opid :: TokenParser Id
opid
  = identifier lexOp
  <?> "operator"

varid :: TokenParser Id
varid
  =   identifier lexId
  <?> "variable"

qualifiedVar :: TokenParser (Id, Id)
qualifiedVar 
  = do{ (m,name) <- lexQualifiedId
      ; return (idFromString m, idFromString name)
      }

bindid :: TokenParser Id
bindid
  = do{ x <- varid
      ; do{ lexeme LexEXCL
          ; return x {- (setSortId SortStrict id) -}
          }
        <|> return x
      }

constructor :: TokenParser Id
constructor
  = conid <|> parens conopid

conopid :: TokenParser Id
conopid
  =   identifier lexConOp
  <|> do{ lexeme LexCOLON; return (idFromString ":") }
  <?> "constructor operator"

conid :: TokenParser Id
conid
  =   identifier lexCon
  <?> "constructor"

qualifiedCon :: TokenParser (Id, Id)
qualifiedCon
  = do{ (m,name) <- lexQualifiedCon
      ; return (idFromString m, idFromString name)
      }

typeid :: TokenParser Id
typeid
  = do{ x <- identifier lexCon
      ; return x -- (setSortId SortType id)
      }
  <?> "type"

typevarid :: TokenParser Id
typevarid
  = do{ x <- identifier lexId
      ; return x -- (setSortId SortType id)
      }

identifier :: TokenParser String -> TokenParser Id
identifier p
  = do{ s <- p
      ; return (idFromString s)
      }

----------------------------------------------------------------
-- Basic parsers
----------------------------------------------------------------

lexeme :: Lexeme -> TokenParser ()
lexeme lex = satisfy f <?> show lex
 where
   f a | a == lex  = Just ()
       | otherwise = Nothing


lexChar :: TokenParser Char
lexChar
  = satisfy (\lex -> case lex of { LexChar c -> Just c; _ -> Nothing })

lexString :: TokenParser String
lexString
  = satisfy (\lex -> case lex of { LexString s -> Just s; _ -> Nothing })

lexDouble :: TokenParser Double
lexDouble
  = satisfy (\lex -> case lex of { LexFloat d -> Just d; _ -> Nothing })

lexInt :: TokenParser Integer
lexInt
  = satisfy (\lex -> case lex of { LexInt i -> Just i; _ -> Nothing })

lexId :: TokenParser String
lexId
  = satisfy (\lex -> case lex of { LexId s -> Just s; _ -> Nothing })

lexQualifiedId :: TokenParser (String, String)
lexQualifiedId
  = satisfy (\lex -> case lex of { LexQualId m x -> Just (m,x); _ -> Nothing })

lexOp :: TokenParser String
lexOp
  = satisfy (\lex -> case lex of { LexOp s -> Just s; _ -> Nothing })

lexCon :: TokenParser String
lexCon
  = satisfy (\lex -> case lex of { LexCon s -> Just s; _ -> Nothing })

lexQualifiedCon :: TokenParser (String, String)
lexQualifiedCon
  = satisfy (\lex -> case lex of { LexQualCon m x -> Just (m,x); _ -> Nothing })

lexConOp :: TokenParser String
lexConOp
  = satisfy (\lex -> case lex of { LexConOp s -> Just s; _ -> Nothing })

satisfy :: (Lexeme -> Maybe a) -> TokenParser a
satisfy p
  = tokenPrim showtok nextpos (\(_,lex) -> p lex)
  where
    showtok (_,lex) = show lex
    nextpos pos _ (((line,col),_):_)
       = setSourceColumn (setSourceLine pos line) col
    nextpos pos _ []
       = pos
