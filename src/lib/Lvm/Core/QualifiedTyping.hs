module Lvm.Core.QualifiedTyping (qualifiedTypes) where

import Lvm.Core.Parsing.Parser
import Lvm.Core.Type
import Lvm.Common.Byte
import Lvm.Common.Id
import Lvm.Common.IdMap
import Lvm.Data
--import qualified Lvm.Core.Module as Module

qualifiedTypes :: Module v -> (Module v)
qualifiedTypes modu = let env = getAllTypeConstructors modu
                          special = map idFromString ["[]", "()"]
                          env0 = foldr deleteMap env special
    in changeAllTypesToQualified env0 modu

getAllTypeConstructors :: Module v -> IdMap Id
getAllTypeConstructors Module{moduleDecls = decls } = foldl addToTypeConEnv emptyMap decls


changeAllTypesToQualified :: IdMap Id -> Module v -> Module v
changeAllTypesToQualified env modu@Module{moduleDecls = decls } = 
    modu{moduleDecls = map mapdecl decls}
        where
            mapcustoms cs = map (changeCustomTypeToQualified env) $! cs

            mapdecl decl@DeclExtern{externType = ty} = case parseTypeFromString ty of
                Left _ -> decl
                Right newty -> decl{externType = show $ qualifyType env newty}
            mapdecl decl@DeclCustom{declKind = k, declCustoms = cs} | k == customTypeDecl = decl{declCustoms = map (changeTypeSynonymToQualified env) cs}
            mapdecl decl = decl{declCustoms = mapcustoms (declCustoms $! decl)}

changeCustomTypeToQualified :: IdMap Id -> Custom -> Custom
changeCustomTypeToQualified env (CustomDecl kind [CustomBytes bs]) | kind == customDeclKind "type" = 
    let oldtype = (parseTypeFromString . stringFromBytes) bs
        newbs = case oldtype of
            Left _ -> bs
            Right ty -> bytesFromString.show $ qualifyType env ty
    in CustomDecl kind [CustomBytes newbs]
changeCustomTypeToQualified _ c = c

changeTypeSynonymToQualified :: IdMap Id -> Custom -> Custom
changeTypeSynonymToQualified env (CustomBytes bs) = 
    let (lhs, _: rhs ) = break (== '=')  (stringFromBytes bs)
        (newlhs, newrhs) = case parseTypeFromString lhs of
            Left _ -> (lhs, rhs)
            Right tyl -> case parseTypeFromString rhs of
                Left _ -> (lhs, rhs)
                Right tyr -> (show (qualifyType env tyl), show (qualifyType env tyr))
        newbs = bytesFromString $ newlhs ++ " = " ++ newrhs
    in CustomBytes newbs
changeTypeSynonymToQualified _ c = c

-- Unfortunately have to perform some strictness, otherwise it breaks. 
-- I suspect the unsafePerformIO is to blame from stringFromId.
addToTypeConEnv :: IdMap Id -> Decl v -> IdMap Id
addToTypeConEnv env decl =
    case decl of
        DeclCustom {declName = n, declKind = k, declCustoms = cs} | k `elem` [customData, customTypeDecl] 
            -> updateMap n (toQualName (getOrigin n cs) n) env
        _   -> env
    where
        toQualName :: Id -> Id -> Id
        toQualName origin name =
            let ori = stringFromId origin
                na = stringFromId name
                newname = ori `seq` na `seq` ori ++ "." ++ na
            in seq (newname) $ idFromString newname

getOrigin :: Id -> [Custom] -> Id
getOrigin n [] =  error ("Core.QualifiedTyping.getOrigin: no origin found for: " ++ show n)
getOrigin n ((CustomDecl kind [CustomName originalmod]):cs) | kind == customOrigin = originalmod
                                                            | otherwise = getOrigin n cs
getOrigin n (_:cs) = getOrigin n cs

mapType :: (Id -> Id) -> Type -> Type
mapType f (TFun t1 t2)      = TFun (mapType f t1) (mapType f t2)
mapType f (TAp t1 t2)       = TAp (mapType f t1) (mapType f t2)
mapType f (TForall ident t) = TForall (f ident) (mapType f t)
mapType f (TExist ident t)  = TExist (f ident) (mapType f t)
mapType f (TStrict t)       = TStrict (mapType f t) 
mapType f (TVar ident)      = TVar (f ident)
mapType f (TCon ident)      = TCon (f ident)
mapType _ TAny              = TAny
mapType _ (TString str)     = TString str

qualifyType :: IdMap Id -> Type -> Type
qualifyType env ty = mapType replace ty
    where
        replace ident = case lookupMap ident env of
            Nothing -> ident
            Just newident -> newident
            