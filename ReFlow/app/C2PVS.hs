module C2PVS where

import Language.C
import Language.C.Data.Ident
import qualified Data.Map as M

import qualified FramaC.Types as FC
import qualified Operators as OP
import qualified FramaC.CLang as RFC
import qualified AbsPVSLang as PVS


type Identifier = String

type Env = M.Map Identifier PVS.PVSType

envLookup :: Env -> Identifier -> PVS.PVSType
envLookup env id = case M.lookup id env of
  (Just ty) -> ty
  Nothing -> error $ show id ++ " : id not found\n"

envInsert :: Env -> Identifier -> PVS.PVSType -> Env
envInsert env id ty = M.insert id ty env

readRational s = toRational (read s :: Float)
  

argDecl :: Show a => Env -> CDeclaration a -> (Identifier, PVS.PVSType)
argDecl env (CDecl [(CTypeSpec ty)] [((Just (CDeclr (Just (Ident id _ _)) _ _ _ _)), Nothing, Nothing)] _) = 
  (id, cTypeSpec ty)
argDecl _ _ = undefined

varDecl :: Show a => Env -> CDeclaration a -> (Identifier, PVS.PVSType, PVS.FAExpr)
varDecl env (CDecl [(CTypeSpec ty)] [((Just (CDeclr (Just (Ident id _ _)) _ _ _ _), (Just (CInitExpr cexpr _)), _))] _) = 
  (id, cTypeSpec ty, cExpression env cexpr)
varDecl _ _ = undefined


topLevel :: Show a => CTranslationUnit a -> [PVS.Decl]
topLevel (CTranslUnit cExternalDeclarations a) = map (funDecl M.empty) cExternalDeclarations

cTypeSpec :: Show a => CTypeSpecifier a -> PVS.PVSType
cTypeSpec (CIntType _) = PVS.TInt
cTypeSpec (CFloatType _) = PVS.FPSingle
cTypeSpec (CDoubleType _) = PVS.FPDouble
cTypeSpec (CBoolType _) = PVS.Boolean
cTypeSpec _               = undefined

typeOf :: Show a => Env -> CExpression a -> PVS.PVSType
typeOf env (CVar (Ident id _ _) _) = envLookup env id
typeOf env (CBinary binOp e1 e2 _) = typeOf env e1
typeOf env (CCast (CDecl [CTypeSpec ty] _ _) (CConst (CFloatConst _ _)) _) = cTypeSpec ty
typeOf env (CConst (CFloatConst _ _)) = PVS.FPDouble
typeOf env expr = error $ show expr


cExpression :: Show a => Env -> CExpression a -> PVS.FAExpr
cExpression env (CBinary binOp e1 e2 _) = PVS.BinaryFPOp (cBinOp binOp) (typeOf env e1) (cExpression env e1) (cExpression env e2)
cExpression env expr@(CVar (Ident id _ _) _) = PVS.FVar (typeOf env expr) id
cExpression env (CConst (CFloatConst (CFloat val) _)) = 
  PVS.FCnst PVS.FPSingle (readRational val)
  where
cExpression env (CConst (CIntConst (CInteger val _ _) _)) =
  PVS.FCnst PVS.TInt (toRational val)
cExpression env (CCast (CDecl [CTypeSpec ty] _ _) (CConst (CFloatConst (CFloat val) _)) _) = 
  PVS.FCnst (cTypeSpec ty) (readRational val)
cExpression _ expr = error $ show expr


cBinOp :: CBinaryOp -> OP.BinOp
cBinOp CAddOp = OP.AddOp
cBinOp CMulOp = OP.MulOp
cBinOp CSubOp = OP.SubOp
cBinOp CDivOp = OP.DivOp


cStatement :: Show a => Env -> CStatement a -> PVS.FAExpr
cStatement env (CCompound [] (cCmpdBlkItm : rest) _) = cCompoundBlockItem env cCmpdBlkItm rest

cCompoundBlockItem :: Show a => Env -> CCompoundBlockItem a -> [CCompoundBlockItem a] -> PVS.FAExpr
cCompoundBlockItem env (CBlockDecl cDecl) (next : rest) = PVS.Let [(id,ty,val)] (cCompoundBlockItem env' next rest)
  where
    (id, ty, val) = varDecl env cDecl
    env' = envInsert env id ty
cCompoundBlockItem env (CBlockStmt (CReturn (Just cexpr) _)) _ = cExpression env cexpr



funDecl :: Show a => Env -> CExternalDeclaration a -> PVS.Decl
funDecl env (CFDefExt (CFunDef [(CTypeSpec retTy)] (CDeclr (Just (Ident fid _ _)) cFunDeclrs _ _ _) _ cStmt _)) = 
  PVS.Decl True (cTypeSpec retTy) fid pvsArgs (cStatement env' cStmt)
  where
    env' = foldl (\env (id, ty) -> envInsert env id ty) env args
    args = concatMap (\(CFunDeclr (Right (cDecls, _)) _ _) -> map (argDecl env) cDecls) cFunDeclrs
    pvsArgs = map (\(id,ty)-> PVS.Arg id ty) args



