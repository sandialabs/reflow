module C2PVS where

import Language.C
import Language.C.Data.Ident

import qualified FramaC.Types as FC
import qualified Operators as OP
import qualified FramaC.CLang as RFC
import qualified AbsPVSLang as PVS


type Identifier = String

argDecl :: Show a => CDeclaration a -> (Identifier, PVS.PVSType)
argDecl (CDecl [(CTypeSpec ty)] [((Just (CDeclr (Just (Ident id _ _)) _ _ _ _)), Nothing, Nothing)] _) = 
  (id, cTypeSpec ty)
argDecl _ = undefined

varDecl :: Show a => CDeclaration a -> (Identifier, PVS.PVSType, PVS.FAExpr)
varDecl (CDecl [(CTypeSpec ty)] [((Just (CDeclr (Just (Ident id _ _)) _ _ _ _), (Just (CInitExpr cexpr _)), _))] _) = 
  (id, cTypeSpec ty, cExpression cexpr)
varDecl _ = undefined


topLevel :: Show a => CTranslationUnit a -> [PVS.Decl]
topLevel (CTranslUnit cExternalDeclarations a) = map funDecl cExternalDeclarations

cTypeSpec :: Show a => CTypeSpecifier a -> PVS.PVSType
cTypeSpec (CIntType _) = PVS.TInt
cTypeSpec (CFloatType _) = PVS.FPSingle
cTypeSpec (CDoubleType _) = PVS.FPDouble
cTypeSpec (CBoolType _) = PVS.Boolean
cTypeSpec _               = undefined


cExpression :: Show a => CExpression a -> PVS.FAExpr
cExpression (CBinary binOp e1 e2 _) = PVS.BinaryFPOp (cBinOp binOp) PVS.FPSingle (cExpression e1) (cExpression e2)
cExpression (CVar (Ident id _ _) _) = PVS.FVar PVS.FPSingle id
cExpression (CConst (CFloatConst (CFloat val) _)) = 
  PVS.FCnst PVS.FPSingle (readRational val)
  where
    readRational s = toRational (read s :: Float)
cExpression (CConst (CIntConst (CInteger val _ _) _)) =
  PVS.FCnst PVS.TInt (toRational val)
cExpression expr = error $ show expr


cBinOp :: CBinaryOp -> OP.BinOp
cBinOp CAddOp = OP.AddOp
cBinOp CMulOp = OP.MulOp
cBinOp CSubOp = OP.SubOp
cBinOp CDivOp = OP.DivOp


cStatement :: Show a => CStatement a -> PVS.FAExpr
cStatement (CCompound [] (cCmpdBlkItm : rest) _) = cCompoundBlockItem  cCmpdBlkItm rest

cCompoundBlockItem :: Show a => CCompoundBlockItem a -> [CCompoundBlockItem a] -> PVS.FAExpr
cCompoundBlockItem (CBlockDecl cDecl) (next : rest) = PVS.Let [(id,ty,val)] (cCompoundBlockItem next rest)
  where
    (id, ty, val) = varDecl cDecl
cCompoundBlockItem (CBlockStmt (CReturn (Just cexpr) _)) _ = cExpression cexpr



funDecl :: Show a => CExternalDeclaration a -> PVS.Decl
funDecl (CFDefExt (CFunDef [(CTypeSpec retTy)] (CDeclr (Just (Ident fid _ _)) cFunDeclrs _ _ _) _ cStmt _)) = 
  PVS.Decl True (cTypeSpec retTy) fid pvsArgs (cStatement cStmt)
  where
    x (CFunDeclr (Right (cDecls, _)) _ _) = map argDecl cDecls
    args = concatMap x cFunDeclrs
    pvsArgs = map pvsArg args
    pvsArg (id, ty) = PVS.Arg id ty
    pvsVars = map (PVS.FVar PVS.FPSingle)



