{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file
-- is distributed under the terms of the BSD3 License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id$

module Lvm.Core.Parsing.Parser
  ( parseModule,
    parseCore,
    pexpr,
    pmodule,
    TokenParser,
    parens,
  )
where

import Control.Monad
import Data.Functor
import Data.List
import Lvm.Common.Byte
import Lvm.Common.Id
import Lvm.Common.IdSet
import Lvm.Core.Expr
import Lvm.Core.Parsing.Lexer
import Lvm.Core.Parsing.Token
  ( Lexeme (..),
    Token,
  )
import Lvm.Core.Type
import Lvm.Core.Utils
import Text.ParserCombinators.Parsec hiding
  ( satisfy,
  )
import Prelude hiding (lex)

parseModule ::
  FilePath ->
  [Token] ->
  IO CoreModule
parseModule fname ts = parseCore fname ts pmodule

parseCore ::
  FilePath ->
  [Token] ->
  TokenParser a ->
  IO a
parseCore fname ts p = case runParser p () fname ts of
  Left err -> ioError (userError ("parse error: " ++ show err))
  Right res -> return res


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

pmodule :: TokenParser CoreModule
pmodule = do
  lexeme LexMODULE
  moduleId <- conid <?> "module name"
  lexeme LexWHERE
  lexeme LexLBRACE
  imports <- pimports <|> return []
  declss <- pdecls
  lexeme LexRBRACE
  lexeme LexEOF
  return $ Module moduleId 0 0 imports $ concat declss

pdecls :: TokenParser [[CoreDecl]]
pdecls = semiList
      ( wrap (ptopDecl <|> pconDecl <|> pabstract <|> pextern <|> pCustomDecl)
          <|> pdata
          <|> ptypeTopDecl
      )

pimports :: TokenParser [Id]
pimports = do
  lexeme LexIMPORT
  imports <- commaParens (conid <?> "module name")
  lexeme LexSEMI
  return imports

----------------------------------------------------------------
-- abstract declarations
----------------------------------------------------------------
pabstract :: TokenParser CoreDecl
pabstract = do
  lexeme LexABSTRACT
  pabstractValue

pabstractValue :: TokenParser (Decl v)
pabstractValue = do
  x  <- pVariableName
  (access, mod, custom) <- pAttributes x pVariableName
  lexeme LexCOLCOL
  t <- ptypeConstraint
  return (DeclAbstract x access mod 0 t custom)

{-pabstractCon :: TokenParser (Decl v)
pabstractCon = do
  x             <- conid
  (acc, custom) <- pAttributes -- ignore access
  lexeme LexASG
  (mid, impid) <- qualifiedCon
  lexeme LexCOLCOL
  t <- ptype
  let access | isImported acc = acc
             | otherwise      = Imported False mid impid DeclKindCon 0 0
  return (DeclCon x access t [] custom)
-}

----------------------------------------------------------------
-- constructor declarations
----------------------------------------------------------------

pconDecl :: TokenParser CoreDecl
pconDecl = do
  lexeme LexCON
  x <- constructor
  (access, mod, custom) <- pAttributes x constructor
  lexeme LexCOLCOL
  t <- ptypeConstraint
  return $ DeclCon x access mod t [] custom

-- constructor info: (@tag, arity)
pConInfo :: TokenParser (Tag, Arity)
pConInfo =
  parens
    ( do
        lexeme LexAT
        tag <- lexInt <?> "tag"
        lexeme LexCOMMA
        arity <- lexInt <?> "arity"
        return (fromInteger tag, fromInteger arity)
    )
    <|> do
      -- :: TypeSig = tag
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
  when (x /= x2) $
    fail
      ( "identifier for type signature "
          ++ stringFromId x
          ++ " doesn't match the definition"
          ++ stringFromId x2
      )
  (access, mod, custom, expr) <- pbindTopRhs x pVariableName
  return (DeclValue x access mod tp expr custom)

pbindTopRhs :: Id -> TokenParser Id -> TokenParser (Access, Maybe Id, [Custom], Expr)
pbindTopRhs name palias =
  do
    (access, mod, custom) <- pAttributes name palias
    lexeme LexASG
    body <- pexpr
    return (access, mod, custom, body)
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
  x <- typeid
  args <- many lexTypeVar
  let kind = foldr (KFun . const KStar) KStar args
      datadecl = DeclCustom x (Export x) Nothing customData [customKind kind]
  do
    lexeme LexASG
    let t1 = foldl TAp (TCon $ TConDataType x) (map TVar args)
    cons <- sepBy1 (pconstructor t1) (lexeme LexBAR)
    let con (cid, t2) = DeclCon cid (Export cid) Nothing t2 [] [CustomLink x customData]
    return (datadecl : map con cons)
    <|> {- empty data types -} return [datadecl]

pconstructor :: Type -> TokenParser (Id, Type)
pconstructor tp = do
  x <- constructor
  args <- many ptypeAtom
  return (x, foldr (\t1 t2 -> TAp (TAp (TCon TConFun) t1) t2) tp args)

----------------------------------------------------------------
-- type declarations
----------------------------------------------------------------

ptypeTopDecl :: TokenParser [CoreDecl]
ptypeTopDecl = do
  lexeme LexTYPE
  x <- typeid
  (access, mod, customs) <- pAttributes x typeid
  -- ; args <- many lexTypeVar
  lexeme LexASG
  tp <- ptype
  return [DeclTypeSynonym x (Export x) Nothing tp []] -- TODO: Handle type arguments

----------------------------------------------------------------
-- Custom
----------------------------------------------------------------
pCustomDecl :: TokenParser CoreDecl
pCustomDecl = do
  lexeme LexCUSTOM
  kind <- pdeclKind
  x <- customid
  (access, mod, customs) <- pAttributes x customid
  return (DeclCustom x access mod kind customs)

pAttributes :: Id -> TokenParser Id -> TokenParser (Access, Maybe Id, [Custom])
pAttributes name palias =
  do
    lexeme LexCOLON
    access <- paccess name palias
    mod <- pfrom
    customs <- pcustoms
    return (access, mod, customs)
    <|> return (Private, Nothing, [])

paccess :: Id -> TokenParser Id -> TokenParser Access
paccess name palias =
  do
    lexeme LexEXPORT
    alias <- palias <|> return name
    return $ Export alias
    <|> return Private

pfrom :: TokenParser (Maybe Id)
pfrom =
  do
    lexeme LexFROM
    mod <- identifier lexString <|> conid
    return (Just mod)
    <|> return Nothing

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
    . fromInteger
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
    . fromInteger
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
    arg <- variable
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
      (kind, idx) <- do
        idx <- lexTypeVar
        return (KStar, idx)
        <|> do
        idx <- lexAnnVar
        return (KAnn, idx)
      lexeme LexDOT
      Forall (Quantor idx kind Nothing) <$> pexpr
    <|> pexprAp
    <?> "expression"

wildId :: Id
wildId = idFromString "_"

pexprAp :: TokenParser Expr
pexprAp = do
  e1 <- patom
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
    <|> (Con . ConId) <$> conid <*> option Nothing (Just <$ lexeme LexAT <*> varid)
    <|> Lit <$> pliteral
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
    cons = Ap . Ap (Con (ConId (idFromString ":")) Nothing)
    nil = Con (ConId (idFromString "[]")) Nothing

parenExpr :: TokenParser Expr
parenExpr = do
  lexeme LexLPAREN
  expr <-
    Var
      <$> opid
      <|> (Con . ConId) <$> conopid <*> option Nothing (Just <$ lexeme LexAT <*> varid)
      <|> (do
        lexeme LexAT
        (do
           arity <- lexInt <?> "arity"
           return (Con (ConTuple (fromInteger arity)) Nothing)) <|> Lit <$> parenLiteral)
      <|> do
        exprs <- pexpr `sepBy` lexeme LexCOMMA
        case exprs of
          [expr] -> return expr
          _ ->
            let con = Con (ConTuple (length exprs)) Nothing
                tup = foldl Ap con exprs
             in return tup
  lexeme LexRPAREN
  return expr

parenLiteral :: TokenParser Literal
parenLiteral =
  lexeme (LexCon "Char") *> pchar lexInt
  <|> lexeme (LexCon "Int") *> pnumber id id
  <|> lexeme LexDASH *> pnumber negate negate

pliteral :: TokenParser Literal
pliteral =
  LitBytes . bytesFromString <$> lexString
  <|>
  pchar lexChar
  <|>
  pnumber id id
  <|> do
  lexeme LexDASH
  pnumber negate negate
 <?> "literal"

pchar :: (Enum a) => TokenParser a -> TokenParser Literal
pchar p = flip (LitInt . fromEnum) IntTypeChar <$> p

pnumber :: (Int -> Int) -> (Double -> Double) -> TokenParser Literal
pnumber signint signdouble =
  do
    i <- lexInt
    return (LitInt (signint (fromInteger i)) IntTypeInt)
    <|>
    LitDouble
    . signdouble
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
      ids <- many bindid
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
  args <- many bindid
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
    linkConv <- plinkConv
    callConv <- pcallConv
    x <- varid
    m <- lexString <|> return (stringFromId x)
    (mname, name) <- pExternName m
    tp <- ptypeDecl
    return (DeclExtern x Private Nothing tp "" linkConv callConv mname name [])
    <|> do
      lexeme LexINSTR
      x <- varid
      s <- lexString
      tp <- ptypeDecl
      return
        (DeclExtern x Private Nothing tp "" LinkStatic CallInstr "" (Plain s) [])

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
  ptypeConstraint

-- We first parse the type of the function, then possible uniqueness constraints follow
ptypeConstraint :: TokenParser Type
ptypeConstraint = ap maybe TQTy <$> ptype <*> optionMaybe (lexeme LexCOMMA *> pTQTy)

pTQTy :: TokenParser [(UAnn, UAnn)]
pTQTy = lexeme LexLBRACKET *> sepBy pConstraint (lexeme LexCOMMA) <* lexeme LexRBRACKET

pConstraint :: TokenParser (UAnn, UAnn)
pConstraint = (,) <$> pUAnn <* lexeme (LexOp "<=") <*> pUAnn

ptype :: TokenParser Type
ptype = ptypeFun <|> do
  lexeme LexFORALL
  (kind, idx) <- (do
    idx <- lexTypeVar
    return (KStar, idx))
    <|> (do
    idx <- lexAnnVar
    return (KAnn, idx))
  lexeme LexDOT
  TForall (Quantor idx kind Nothing) <$> ptype

ptypeFun :: TokenParser Type
ptypeFun = chainr1 ptypeAp pFun
  where
    pFun = ((TAp .) . TAp) <$ lexeme LexRARROW <*> pArrow

pArrow :: TokenParser Type
pArrow = option arr (flip addUAnnToType arr <$ lexeme LexCOLON <*> pUAnn)
  where arr = TCon TConFun

ptypeAp :: TokenParser Type
ptypeAp = do
  atoms <- many1 ptypeAtom
  return (foldl1 TAp atoms)

ptypeAtom :: TokenParser Type
ptypeAtom = ptypeAnn ptypeAtom'

ptypeAtom' :: TokenParser Type
ptypeAtom' =
  do
    x <- typeid
    return (TCon $ TConDataType x)
    <|> do
      x <- lexTypeVar
      return (TVar x)
    <|> listType
    <|> (lexeme LexLPAREN *> parenType <* lexeme LexRPAREN)
    <?> "atomic type"

-- We first parse a optional strictness annotation
-- Then we parse the other annotations, followed by parsing the type
ptypeAnn :: TokenParser Type -> TokenParser Type
ptypeAnn p = (typeToStrict <$ lexeme LexEXCL <*> ptypeUAnn p) <|> ptypeUAnn p

ptypeUAnn :: TokenParser Type -> TokenParser Type
ptypeUAnn p = (addUAnnToType <$> pUAnn <* lexeme LexCOLON <*> p) <|> p

pUAnn :: TokenParser UAnn
pUAnn = (UShared <$ lexeme (LexId "w") <|> UUnique <$ lexeme (LexInt 1) <|> (UVar <$> lexAnnVar))

parenType :: TokenParser Type
parenType =
  (do
   lexeme LexAT
   pparenTuple <|> pparenDictionary)
  <|> ptype
  <|> do
    arity <- length <$> many (lexeme LexCOMMA)
    case arity of
      0 -> return $ TCon $ TConTuple $ 0
      _ -> return $ TCon $ TConTuple (arity + 1)

pparenDictionary :: TokenParser Type
pparenDictionary = do
  lexeme (LexId "dictionary")
  TCon . TConTypeClassDictionary <$> typeid

pparenTuple :: TokenParser Type
pparenTuple = do
  lexeme LexCOMMA
  commas <- many (lexeme LexCOMMA)
  let arity = length commas + 2
  return $ TCon $ TConTuple $ fromIntegral arity

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
  Variable name <$> parens ptype

opid :: TokenParser Id
opid = identifier lexOp <?> "operator"

varid :: TokenParser Id
varid = identifier lexId <?> "variable"

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

typeid :: TokenParser Id
typeid =
  identifier lexCon
    <?> "type"

identifier :: TokenParser String -> TokenParser Id
identifier = fmap idFromString

----------------------------------------------------------------
-- Basic parsers
----------------------------------------------------------------

lexeme :: Lexeme -> TokenParser ()
lexeme lex = satisfy f <?> show lex
  where
    f a
      | a == lex = Just ()
      | otherwise = Nothing

lexChar :: TokenParser Char
lexChar =
  satisfy
    ( \case
        LexChar c -> Just c
        _ -> Nothing
    )

lexString :: TokenParser String
lexString =
  satisfy
    ( \case
        LexString s -> Just s
        _ -> Nothing
    )

lexDouble :: TokenParser Double
lexDouble =
  satisfy
    ( \case
        LexFloat d -> Just d
        _ -> Nothing
    )

lexInt :: TokenParser Integer
lexInt =
  satisfy
    ( \case
        LexInt i -> Just i
        _ -> Nothing
    )

lexId :: TokenParser String
lexId =
  satisfy
    ( \case
        LexId s -> Just s
        _ -> Nothing
    )

lexTypeVar :: TokenParser Int
lexTypeVar =
  satisfy
    ( \case
        LexTypeVar idx -> Just idx
        _ -> Nothing
    )

lexAnnVar :: TokenParser Int
lexAnnVar =
  satisfy
    ( \case
        LexAnnVar idx -> Just idx
        _ -> Nothing
    )

lexOp :: TokenParser String
lexOp =
  satisfy
    ( \case
        LexOp s -> Just s
        _ -> Nothing
    )

lexCon :: TokenParser String
lexCon =
  satisfy
    ( \case
        LexCon s -> Just s
        _ -> Nothing
    )

lexConOp :: TokenParser String
lexConOp =
  satisfy
    ( \case
        LexConOp s -> Just s
        _ -> Nothing
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
   in case runParser (waitForEOF p) () "Core.Parsing.Parser" toks of
        Left _ -> Left str --error ("Core.Parsing.Parser parseFromString: parse error in " ++ string ++ show tokens)
        Right x -> Right x

waitForEOF :: TokenParser a -> TokenParser a
waitForEOF p = do
  x <- p
  lexeme LexEOF
  return x
