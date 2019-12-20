--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file
-- is distributed under the terms of the BSD3 License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id$

{-# LANGUAGE LambdaCase #-}

module Lvm.Core.Parsing.Parser
  ( parseModuleExport
  , parseModule
  )
where

import           Control.Monad
import           Data.List
import           Data.Functor
import           Lvm.Common.Byte
import           Lvm.Common.Id
import           Lvm.Common.IdSet
import           Lvm.Core.Expr
import           Lvm.Core.Parsing.Token         ( Token
                                                , Lexeme(..)
                                                )
import           Lvm.Core.Parsing.Lexer
import           Lvm.Core.Type
import           Lvm.Core.Utils
import           Prelude                 hiding ( lex )
import           Text.ParserCombinators.Parsec
                                         hiding ( satisfy )

parseModuleExport
  :: FilePath
  -> [Token]
  -> IO (CoreModule, Bool, (IdSet, IdSet, IdSet, IdSet, IdSet))
parseModuleExport fname ts = case runParser pmodule () fname ts of
  Left  err -> ioError (userError ("parse error: " ++ show err))
  Right res -> return res

parseModule :: FilePath -> [Token] -> IO CoreModule
parseModule fname = fmap (\(m, _, _) -> m) . parseModuleExport fname

----------------------------------------------------------------
-- Basic parsers
----------------------------------------------------------------
type TokenParser a = GenParser Token () a

----------------------------------------------------------------
-- Program
----------------------------------------------------------------

wrap :: TokenParser a -> TokenParser [a]
wrap p = do
  x <- p
  return [x]

pmodule :: TokenParser (CoreModule, Bool, (IdSet, IdSet, IdSet, IdSet, IdSet))
pmodule = do
  lexeme LexMODULE
  moduleId <- conid <?> "module name"
  exports  <- pexports
  lexeme LexWHERE
  lexeme LexLBRACE
  declss_ <- semiList
    (   wrap (ptopDecl <|> pconDecl <|> pabstract <|> pextern <|> pCustomDecl)
    <|> pdata
    <|> pimport
    <|> ptypeTopDecl
    )
  let declss = map (addOrigininDecl moduleId) (concat declss_)
  lexeme LexRBRACE
  lexeme LexEOF
  return $ case exports of
    Nothing ->
      let es = (emptySet, emptySet, emptySet, emptySet, emptySet)
      in  (modulePublic True es (Module moduleId 0 0 declss), True, es)
    Just es -> (modulePublic False es (Module moduleId 0 0 declss), False, es)

addOrigininDecl :: Id -> CoreDecl -> CoreDecl
addOrigininDecl originalmod decl =
  let cs         = declCustoms decl
      makeOrigin = [CustomDecl customOrigin [CustomName originalmod]]
  in  decl { declCustoms = cs ++ makeOrigin }
----------------------------------------------------------------
-- export list
----------------------------------------------------------------
data Export  = ExportValue Id
             | ExportCon   Id
             | ExportData  Id
             | ExportDataCon Id
             | ExportModule Id

pexports :: TokenParser (Maybe (IdSet, IdSet, IdSet, IdSet, IdSet))
pexports = do
  exports <- commaParens pexport <|> return []
  return $ if null (concat exports)
    then Nothing
    else Just
      (foldl' split
              (emptySet, emptySet, emptySet, emptySet, emptySet)
              (concat exports)
      )
 where
  split (values, cons, datas, datacons, ms) export = case export of
    ExportValue   x -> (insertSet x values, cons, datas, datacons, ms)
    ExportCon     x -> (values, insertSet x cons, datas, datacons, ms)
    ExportData    x -> (values, cons, insertSet x datas, datacons, ms)
    ExportDataCon x -> (values, cons, datas, insertSet x datacons, ms)
    ExportModule  x -> (values, cons, datas, datacons, insertSet x ms)

pexport :: TokenParser [Export]
pexport =
  do
      lexeme LexLPAREN
      entity <- ExportValue <$> opid <|> do
        ExportCon <$> conopid
      lexeme LexRPAREN
      return [entity]
    <|> do
          x <- varid
          return [ExportValue x]
    <|> do
          x <- typeid
          do
              lexeme LexLPAREN
              cons <- pexportCons x
              lexeme LexRPAREN
              return (ExportData x : cons)
            <|>
          -- no parenthesis: could be either a
          -- constructor or a type constructor
                return [ExportData x, ExportCon x]
    <|> do
          lexeme LexMODULE
          x <- conid
          return [ExportModule x]

pexportCons :: Id -> TokenParser [Export]
pexportCons x =
  do
      lexeme LexDOTDOT
      return [ExportDataCon x]
    <|> do
          xs <- sepBy constructor (lexeme LexCOMMA)
          return (map ExportCon xs)


----------------------------------------------------------------
-- abstract declarations
----------------------------------------------------------------
pabstract :: TokenParser CoreDecl
pabstract = do
  lexeme LexABSTRACT
  pabstractValue <|> pabstractCon

pabstractValue :: TokenParser (Decl v)
pabstractValue = do
  x             <- pVariableName
  (acc, custom) <- pAttributes private
  lexeme LexCOLON
  arity <- lexInt
  lexeme LexASG
  (mid, impid) <- qualifiedVar
  t            <- ptypeDecl
  let access | isImported acc = acc
             | otherwise      = Imported False mid impid DeclKindValue 0 0
  return (DeclAbstract x access (fromIntegral arity) t custom)

pabstractCon :: TokenParser (Decl v)
pabstractCon = do
  x             <- conid
  (acc, custom) <- pAttributes private -- ignore access
  lexeme LexASG
  (mid, impid) <- qualifiedCon
  lexeme LexCOLCOL
  t <- ptype
  let access | isImported acc = acc
             | otherwise      = Imported False mid impid DeclKindCon 0 0
  return (DeclCon x access t [] custom)

isImported :: Access -> Bool
isImported Imported{} = True
isImported _          = False

----------------------------------------------------------------
-- import declarations
----------------------------------------------------------------
pimport :: TokenParser [CoreDecl]
pimport = do
  lexeme LexIMPORT
  mid <- conid
  do
      xss <- commaParens (pImportSpec mid)
      return (concat xss)
    <|> return
          [DeclImport mid (Imported False mid dummyId DeclKindModule 0 0) []]

pImportSpec :: Id -> TokenParser [CoreDecl]
pImportSpec mid =
  do
      lexeme LexLPAREN
      (kind, x) <-
        do
            y <- opid
            return (DeclKindValue, y)
          <|> do
                y <- conopid
                return (DeclKindCon, y)
      lexeme LexRPAREN
      impid <- option
        x
        (do
          lexeme LexASG
          pVariableName
        )
      return [DeclImport x (Imported False mid impid kind 0 0) []]
    <|> do
          x     <- varid
          impid <- option
            x
            (do
              lexeme LexASG
              pVariableName
            )
          return [DeclImport x (Imported False mid impid DeclKindValue 0 0) []]
    <|> do
          lexeme LexCUSTOM
          kind  <- lexString
          x     <- pVariableName <|> constructor
          impid <- option
            x
            (do
              lexeme LexASG
              pVariableName <|> constructor
            )
          return
            [ DeclImport x
                         (Imported False mid impid (customDeclKind kind) 0 0)
                         []
            ]
    <|> do
          lexeme LexTYPE
          x     <- constructor
          impid <- option
            x
            (do
              lexeme LexASG
              pVariableName <|> constructor
            )
          return
            [DeclImport x (Imported False mid impid DeclKindTypeSynonym 0 0) []]
    <|> do
          x     <- typeid
          impid <- option
            x
            (do
              lexeme LexASG
              pVariableName
            )
          do
              lexeme LexLPAREN
              cons <- pImportCons mid
              lexeme LexRPAREN
              return
                ( DeclImport x (Imported False mid impid customData 0 0) []
                : cons
                )
            <|> return
                  [DeclImport x (Imported False mid impid DeclKindCon 0 0) []]

pImportCons :: Id -> TokenParser [CoreDecl]
pImportCons mid = -- do{ lexeme LexDOTDOT
    --   ; return [ExportDataCon id]
    --   }
  -- <|>
  sepBy (pimportCon mid) (lexeme LexCOMMA)

pimportCon :: Id -> TokenParser CoreDecl
pimportCon mid = do
  x     <- constructor
  impid <- option
    x
    (do
      lexeme LexASG
      pVariableName
    )
  return (DeclImport x (Imported False mid impid DeclKindCon 0 0) [])

----------------------------------------------------------------
-- constructor declarations
----------------------------------------------------------------

pconDecl :: TokenParser CoreDecl
pconDecl = do
  lexeme LexCON
  x                <- constructor
  (access, custom) <- pAttributes public
  lexeme LexCOLCOL
  t <- ptype
  return $ DeclCon x access t [] custom

-- constructor info: (@tag, arity)
pConInfo :: TokenParser (Tag, Arity)
pConInfo =
  parens
      (do
        lexeme LexAT
        tag <- lexInt <?> "tag"
        lexeme LexCOMMA
        arity <- lexInt <?> "arity"
        return (fromInteger tag, fromInteger arity)
      )
    <|> do -- :: TypeSig = tag
          tp <- ptypeDecl
          lexeme LexASG
          tag <- lexInt <?> "tag"
          return (fromInteger tag, arityFromType tp)


----------------------------------------------------------------
-- value declarations
----------------------------------------------------------------

ptopDecl :: TokenParser CoreDecl
ptopDecl = do
  x <- pVariableName
  ptopDeclType x

ptopDeclType :: Id -> TokenParser (Decl Expr)
ptopDeclType x = do
  tp <- ptypeDecl
  lexeme LexSEMI
  x2 <- pVariableName
  when (x /= x2) $ fail
    (  "identifier for type signature "
    ++ stringFromId x
    ++ " doesn't match the definition"
    ++ stringFromId x2
    )
  (access, custom, expr) <- pbindTopRhs
  return (DeclValue x access tp expr custom)

pbindTopRhs :: TokenParser (Access, [Custom], Expr)
pbindTopRhs =
  do
      (access, custom) <- pAttributes public
      lexeme LexASG
      body <- pexpr
      return (access, custom, body)
    <?> "declaration"



pbind :: TokenParser Bind
pbind = do
  var <- variable
  lexeme LexASG
  Bind var <$> pexpr

----------------------------------------------------------------
-- data declarations
----------------------------------------------------------------

makeCustomBytes :: String -> Bytes -> Custom
makeCustomBytes k bs = CustomDecl (customDeclKind k) [CustomBytes bs]

customKind :: Kind -> Custom
customKind = makeCustomBytes "kind" . bytesFromString . show

pdata :: TokenParser [CoreDecl]
pdata = do
  lexeme LexDATA
  x    <- typeid
  args <- many lexTypeVar
  let kind     = foldr (KFun . const KStar) KStar args
      datadecl = DeclCustom x public customData [customKind kind]
  do
      lexeme LexASG
      let t1 = foldl TAp (TCon $ TConDataType x) (map TVar args)
      cons <- sepBy1 (pconstructor t1) (lexeme LexBAR)
      let con (cid, t2) = DeclCon cid public t2 [] [CustomLink x customData]
      return (datadecl : map con cons)
    <|> {- empty data types -}
        return [datadecl]

pconstructor :: Type -> TokenParser (Id, Type)
pconstructor tp = do
  x    <- constructor
  args <- many ptypeAtom
  return (x, foldr (\t1 t2 -> TAp (TAp (TCon TConFun) t1) t2) tp args)

----------------------------------------------------------------
-- type declarations
----------------------------------------------------------------

ptypeTopDecl :: TokenParser [CoreDecl]
ptypeTopDecl = do
  lexeme LexTYPE
  x <- typeid
-- ; args <- many lexTypeVar
  lexeme LexASG
  tp <- ptype
  return [DeclTypeSynonym x private tp []] -- TODO: Handle type arguments

----------------------------------------------------------------
-- Custom
----------------------------------------------------------------
pCustomDecl :: TokenParser CoreDecl
pCustomDecl = do
  lexeme LexCUSTOM
  kind              <- pdeclKind
  x                 <- customid
  (access, customs) <- pAttributes private
  return (DeclCustom x access kind customs)

pAttributes :: Access -> TokenParser (Access, [Custom])
pAttributes defAccess =
  do
      lexeme LexCOLON
      access  <- paccess defAccess
      customs <- pcustoms
      return (access, customs)
    <|> return (private, [])

paccess :: Access -> TokenParser Access
paccess defAccess =
  do
      lexeme LexPRIVATE
      pimportaccess False <|> return private
    <|> do
          lexeme LexPUBLIC
          pimportaccess True <|> return public
    <|> return defAccess

pimportaccess :: Bool -> TokenParser Access
pimportaccess isPublic = do
  lexeme LexIMPORT
  kind   <- pdeclKind
  (m, x) <- lexQualifiedId
  return $ Imported isPublic (idFromString m) (idFromString x) kind 0 0

pcustoms :: TokenParser [Custom]
pcustoms =
  do
      lexeme LexLBRACKET
      customs <- pcustom `sepBy` lexeme LexCOMMA
      lexeme LexRBRACKET
      return customs
    <|> return []

pcustom :: TokenParser Custom
pcustom =

  CustomInt
    .   fromInteger
    <$> lexInt
    <|> do
          CustomBytes . bytesFromString <$> lexString
    <|> do
          x <- pVariableName <|> constructor
          return (CustomName x)
    <|> do
          lexeme LexNOTHING
          return CustomNothing
    <|> do
          lexeme LexCUSTOM
          kind <- pdeclKind
          do
              x <- customid
              return (CustomLink x kind)
            <|> do
                  CustomDecl kind <$> pcustoms
    <?> "custom value"

pdeclKind :: TokenParser DeclKind
pdeclKind =
  makeDeclKind
    <$> varid
    <|> do
          lexeme LexTYPE
          return DeclKindTypeSynonym
    <|> toEnum
    .   fromInteger
    <$> lexInt
    <|> customDeclKind
    <$> lexString
    <?> "custom kind"

----------------------------------------------------------------
-- Expressions
----------------------------------------------------------------

pexpr :: TokenParser Expr
pexpr =
  do
      lexeme LexBSLASH
      strict <- lexeme LexEXCL $> True <|> return False
      arg    <- variable
      lexeme LexRARROW
      Lam strict arg <$> pexpr
    <|> do
          lexeme LexLET
          binds <- semiBraces pbind
          lexeme LexIN
          Let (Rec binds) <$> pexpr
    <|> do
          lexeme LexCASE
          var <- pVariableName
          lexeme LexOF
          Match var <$> palts
    <|> do
          lexeme LexLETSTRICT
          binds <- semiBraces pbind
          lexeme LexIN
          expr <- pexpr
          return (foldr (Let . Strict) expr binds)
    <|> do
          lexeme LexFORALL
          idx <- lexTypeVar
          let kind = KStar
          lexeme LexDOT
          Forall (Quantor idx Nothing) kind <$> pexpr
    <|> pexprAp
    <?> "expression"

wildId :: Id
wildId = idFromString "_"

pexprAp :: TokenParser Expr
pexprAp = do
  e1   <- patom
  args <- many pApArg
  return (foldl (flip id) e1 args)

pApArg :: TokenParser (Expr -> Expr)
pApArg =
  do
      atom <- patom
      return (`Ap` atom)
    <|> do
          lexeme LexLBRACE
          tp <- ptype
          lexeme LexRBRACE
          return (`ApType` tp)

patom :: TokenParser Expr
patom =
  Var
    <$> varid
    <|> Con
    .   ConId
    <$> conid
    <|> Lit
    <$> pliteral
    <|> parenExpr
    <|> listExpr
    <?> "atomic expression"


listExpr :: TokenParser Expr
listExpr = do
  lexeme LexLBRACKET
  exprs <- sepBy pexpr (lexeme LexCOMMA)
  lexeme LexRBRACKET
  return (foldr cons nil exprs)
 where
  cons = Ap . Ap (Con (ConId (idFromString ":")))
  nil  = Con (ConId (idFromString "[]"))

parenExpr :: TokenParser Expr
parenExpr = do
  lexeme LexLPAREN
  expr <-
    Var
    <$> opid
    <|> Con
    .   ConId
    <$> conopid
    <|> do
          lexeme LexAT
          arity <- lexInt <?> "arity"
          return (Con (ConTuple (fromInteger arity)))
    <|> do
          exprs <- pexpr `sepBy` lexeme LexCOMMA
          case exprs of
            [expr] -> return expr
            _ ->
              let con = Con (ConTuple (length exprs))
                  tup = foldl Ap con exprs
              in  return tup
  lexeme LexRPAREN
  return expr

pliteral :: TokenParser Literal
pliteral =
  pnumber id id
    <|> LitBytes
    .   bytesFromString
    <$> lexString
    <|> do
          c <- lexChar
          return (LitInt (fromEnum c) IntTypeChar)
    <|> do
          lexeme LexDASH
          pnumber negate negate
    <?> "literal"

pnumber :: (Int -> Int) -> (Double -> Double) -> TokenParser Literal
pnumber signint signdouble =
  do
      i <- lexInt
      return (LitInt (signint (fromInteger i)) IntTypeInt)
    <|> LitDouble
    .   signdouble
    <$> lexDouble

----------------------------------------------------------------
-- alternatives
----------------------------------------------------------------
palts :: TokenParser Alts
palts = do
  lexeme LexLBRACE
  paltSemis

paltSemis :: TokenParser Alts
paltSemis =
  do
      alt <- paltDefault
      optional (lexeme LexSEMI)
      lexeme LexRBRACE
      return [alt]
    <|> do
          alt <- palt
          do
              lexeme LexSEMI
              do
                  alts <- paltSemis
                  return (alt : alts)
                <|> do
                      lexeme LexRBRACE
                      return [alt]
            <|> do
                  lexeme LexRBRACE
                  return [alt]

palt :: TokenParser Alt
palt = do
  pat <- ppat
  lexeme LexRARROW
  Alt pat <$> pexpr

pInstantiation :: TokenParser [Type]
pInstantiation = many (lexeme LexLBRACE *> ptype <* lexeme LexRBRACE)

ppat :: TokenParser Pat
ppat = ppatCon <|> ppatLit <|> ppatParens

ppatParens :: TokenParser Pat
ppatParens = do
  lexeme LexLPAREN
  do
      lexeme LexAT
      arity <- lexInt <?> "arity"
      lexeme LexRPAREN
      ids <- many bindid
      return (PatCon (ConTuple (fromInteger arity)) [] ids)
    <|> do
          x <- conopid
          lexeme LexRPAREN
          instantiation <- pInstantiation
          ids           <- many bindid
          return (PatCon (ConId x) instantiation ids)
    <|> do
          pat <- ppat <|> ppatTuple
          lexeme LexRPAREN
          return pat

ppatCon :: TokenParser Pat
ppatCon = do
  x <- conid <|> do
    lexeme LexLBRACKET
    lexeme LexRBRACKET
    return (idFromString "[]")
  instantiation <- pInstantiation
  args          <- many bindid
  return (PatCon (ConId x) instantiation args)

ppatLit :: TokenParser Pat
ppatLit = PatLit <$> pliteral

ppatTuple :: TokenParser Pat
ppatTuple = do
  ids <- bindid `sepBy` lexeme LexCOMMA
  return (PatCon (ConTuple (length ids)) [] ids)

paltDefault :: TokenParser Alt
paltDefault = do
  lexeme LexDEFAULT
  lexeme LexRARROW
  Alt PatDefault <$> pexpr

wildcard :: TokenParser Id
wildcard = identifier (return "_")

----------------------------------------------------------------
-- externs
----------------------------------------------------------------
pextern :: TokenParser CoreDecl
pextern =
  do
      lexeme LexEXTERN
      linkConv      <- plinkConv
      callConv      <- pcallConv
      x             <- varid
      m             <- lexString <|> return (stringFromId x)
      (mname, name) <- pExternName m
      tp            <- ptypeDecl
      return (DeclExtern x private tp "" linkConv callConv mname name [])
    <|> do
          lexeme LexINSTR
          x  <- varid
          s  <- lexString
          tp <- ptypeDecl
          return
            (DeclExtern x private tp "" LinkStatic CallInstr "" (Plain s) [])

------------------

plinkConv :: TokenParser LinkConv
plinkConv =
  do
      lexeme LexSTATIC
      return LinkStatic
    <|> do
          lexeme LexDYNAMIC
          return LinkDynamic
    <|> do
          lexeme LexRUNTIME
          return LinkRuntime
    <|> return LinkStatic

pcallConv :: TokenParser CallConv
pcallConv =
  do
      lexeme LexCCALL
      return CallC
    <|> do
          lexeme LexSTDCALL
          return CallStd
    <|> do
          lexeme LexINSTRCALL
          return CallInstr
    <|> return CallC

pExternName :: String -> TokenParser (String, ExternName)
pExternName mname =
  do
      lexeme LexDECORATE
      name <- lexString
      return (mname, Decorate name)
    <|> do
          lexeme LexORDINAL
          ord <- lexInt
          return (mname, Ordinal (fromIntegral ord))
    <|> do
          name <- lexString
          return (mname, Plain name)
    <|> return ("", Plain mname)


----------------------------------------------------------------
-- types
----------------------------------------------------------------

ptypeDecl :: TokenParser Type
ptypeDecl = do
  lexeme LexCOLCOL
  ptypeNormal

ptypeNormal :: TokenParser Type
ptypeNormal = ptype


ptype :: TokenParser Type
ptype = ptypeFun <|> do
  lexeme LexFORALL
  idx <- lexTypeVar
  let kind = KStar
  lexeme LexDOT
  TForall (Quantor idx Nothing) kind <$> ptype

ptypeFun :: TokenParser Type
ptypeFun = chainr1 ptypeAp pFun
 where
  pFun = do
    lexeme LexRARROW
    return (\t1 t2 -> TAp (TAp (TCon TConFun) t1) t2)

ptypeAp :: TokenParser Type
ptypeAp = do
  atoms <- many1 ptypeAtom
  return (foldl1 TAp atoms)

ptypeAtom :: TokenParser Type
ptypeAtom =
  do
      x <- typeid
      ptypeStrict (TCon $ TConDataType x)
    <|> do
          x <- lexTypeVar
          let t = TVar x
          ptypeStrict t
    <|> (listType >>= ptypeStrict)
    <|> ((lexeme LexLPAREN *> parenType <* lexeme LexRPAREN) >>= ptypeStrict)
    <?> "atomic type"

ptypeStrict :: Type -> TokenParser Type
ptypeStrict tp =
  do
      lexeme LexEXCL
      return (TStrict tp)
    <|> return tp

parenType :: TokenParser Type
parenType =
  do
      lexeme LexAT
      lexeme LexCOMMA
      commas <- many (lexeme LexCOMMA)
      let arity = length commas + 2
      return $ TCon $ TConTuple $ fromIntegral arity
    <|> do
          lexeme LexAT
          lexeme (LexId "dictionary")
          TCon . TConTypeClassDictionary <$> typeid
    <|> do
          tps <- sepBy ptype (lexeme LexCOMMA)
          case tps of
            [tp] -> return tp
            _    -> return (foldl TAp (TCon $ TConTuple $ length tps) tps)

listType :: TokenParser Type
listType = do
  lexeme LexLBRACKET
  do
      tp <- ptype
      lexeme LexRBRACKET
      x <- identifier (return "[]")
      return (TAp (TCon $ TConDataType x) tp)
    <|> do
          lexeme LexRBRACKET
          x <- identifier (return "[]")
          return (TCon $ TConDataType x)

----------------------------------------------------------------
-- helpers
----------------------------------------------------------------

semiBraces, commaParens :: TokenParser a -> TokenParser [a]
semiBraces p = braces (semiList p)
commaParens p = parens (sepBy p (lexeme LexCOMMA))

braces, parens :: TokenParser a -> TokenParser a
braces = between (lexeme LexLBRACE) (lexeme LexRBRACE)
parens = between (lexeme LexLPAREN) (lexeme LexRPAREN)

-- terminated or separated
semiList1 :: TokenParser a -> TokenParser [a]
semiList1 p = do
  x <- p
  do
      lexeme LexSEMI
      xs <- semiList p
      return (x : xs)
    <|> return [x]

semiList :: TokenParser a -> TokenParser [a]
semiList p = semiList1 p <|> return []

----------------------------------------------------------------
-- Lexeme parsers
----------------------------------------------------------------

customid :: TokenParser Id
customid =
  varid
    <|> conid
    <|> parens (opid <|> conopid)
    <|> idFromString
    <$> lexString
    <?> "custom identifier"

pVariableName :: TokenParser Id
pVariableName = varid <|> parens opid

variable :: TokenParser Variable
variable = do
  name <- pVariableName
  lexeme LexCOLON
  Variable name <$> ptypeAp

opid :: TokenParser Id
opid = identifier lexOp <?> "operator"

varid :: TokenParser Id
varid = identifier lexId <?> "variable"

qualifiedVar :: TokenParser (Id, Id)
qualifiedVar = do
  (m, name) <- lexQualifiedId
  return (idFromString m, idFromString name)

bindid :: TokenParser Id
bindid = varid {-
  = do{ x <- varid
      ; do{ lexeme LexEXCL
          ; return x {- (setSortId SortStrict id) -}
          }
        <|> return x
      -}

constructor :: TokenParser Id
constructor = conid <|> parens conopid

conopid :: TokenParser Id
conopid =
  identifier lexConOp
    <|> do
          lexeme LexCOLON
          return (idFromString ":")
    <?> "constructor operator"

conid :: TokenParser Id
conid = identifier lexCon <?> "constructor"

qualifiedCon :: TokenParser (Id, Id)
qualifiedCon = do
  (m, name) <- lexQualifiedCon
  return (idFromString m, idFromString name)

typeid :: TokenParser Id
typeid =
  identifier lexCon
    <|> -- (setSortId SortType id)
        do
          (m, name) <- lexQualifiedCon
          return (idFromString (m ++ "." ++ name))
    <?> "type"

identifier :: TokenParser String -> TokenParser Id
identifier = fmap idFromString

----------------------------------------------------------------
-- Basic parsers
----------------------------------------------------------------

lexeme :: Lexeme -> TokenParser ()
lexeme lex = satisfy f <?> show lex
 where
  f a | a == lex  = Just ()
      | otherwise = Nothing


lexChar :: TokenParser Char
lexChar = satisfy
  (\case
    LexChar c -> Just c
    _         -> Nothing
  )

lexString :: TokenParser String
lexString = satisfy
  (\case
    LexString s -> Just s
    _           -> Nothing
  )

lexDouble :: TokenParser Double
lexDouble = satisfy
  (\case
    LexFloat d -> Just d
    _          -> Nothing
  )

lexInt :: TokenParser Integer
lexInt = satisfy
  (\case
    LexInt i -> Just i
    _        -> Nothing
  )

lexId :: TokenParser String
lexId = satisfy
  (\case
    LexId s -> Just s
    _       -> Nothing
  )

lexTypeVar :: TokenParser Int
lexTypeVar = satisfy
  (\case
    LexTypeVar idx -> Just idx
    _              -> Nothing
  )

lexQualifiedId :: TokenParser (String, String)
lexQualifiedId = satisfy
  (\case
    LexQualId m x -> Just (m, x)
    _             -> Nothing
  )

lexOp :: TokenParser String
lexOp = satisfy
  (\case
    LexOp s -> Just s
    _       -> Nothing
  )

lexCon :: TokenParser String
lexCon = satisfy
  (\case
    LexCon s -> Just s
    _        -> Nothing
  )

lexQualifiedCon :: TokenParser (String, String)
lexQualifiedCon = satisfy
  (\case
    LexQualCon m x -> Just (m, x)
    _              -> Nothing
  )

lexConOp :: TokenParser String
lexConOp = satisfy
  (\case
    LexConOp s -> Just s
    _          -> Nothing
  )

satisfy :: (Lexeme -> Maybe a) -> TokenParser a
satisfy p = tokenPrim showtok nextpos (\(_, lex) -> p lex)
 where
  showtok (_, lex) = show lex
  nextpos pos _ (((line, col), _) : _) =
    setSourceColumn (setSourceLine pos line) col
  nextpos pos _ [] = pos

parseFromString :: TokenParser a -> String -> Either String a
parseFromString p str =
  let toks = lexer (0, 0) str
  in  case runParser (waitForEOF p) () "Core.Parsing.Parser" toks of
        Left  _ -> Left str --error ("Core.Parsing.Parser parseFromString: parse error in " ++ string ++ show tokens)
        Right x -> Right x

waitForEOF :: TokenParser a -> TokenParser a
waitForEOF p = do
  x <- p
  lexeme LexEOF
  return x
