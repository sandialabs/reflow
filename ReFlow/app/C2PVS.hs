module C2PVS where

import Language.C

import qualified FramaC.Types as FC
import qualified FramaC.CLang as RFC
import qualified AbsPVSLang as PVS

-- data CTypeSpecifier a
--   = CVoidType    a
--   | CCharType    a
--   | CShortType   a
--   | CIntType     a
--   | CLongType    a
--   | CFloatType   a
--   | CDoubleType  a
--   | CSignedType  a
--   | CUnsigType   a
--   | CBoolType    a
--   | CComplexType a
--   | CInt128Type  a
--   | CUInt128Type a
--   | CFloatNType Int Bool a           -- ^ IEC 60227: width (32,64,128), extended flag
--   | CSUType      (CStructureUnion a) a      -- ^ Struct or Union specifier
--   | CEnumType    (CEnumeration a)    a      -- ^ Enumeration specifier
--   | CTypeDef     Ident        a      -- ^ Typedef name
--   | CTypeOfExpr  (CExpression a)  a  -- ^ @typeof(expr)@
--   | CTypeOfType  (CDeclaration a) a  -- ^ @typeof(type)@
--   | CAtomicType  (CDeclaration a) a  -- ^ @_Atomic(type)@
--     deriving (Show, Data,Typeable, Generic, Generic1 {-! ,CNode ,Functor ,Annotated !-})

-- instance NFData a => NFData (CTypeSpecifier a)

cExprToFAExpr :: CExpression a -> PVS.FAExpr
cExprToFAExpr (CAssign CAssignOp (CVar id _) cexpr _) = undefined



-- transType :: CTypeSpec -> FC.Type
-- transType ty = case ty of
--     (CFloatType _) -> FC.FPSingle
--     (CDoubleType _) -> FC.FPDouble
--     (CBoolType _) -> FC.Boolean
--     (CIntType _) -> FC.Tint
--     _ -> undefined

-- -- data CExpression a
-- --   = CComma       [CExpression a]         -- comma expression list, n >= 2
-- --                  a
-- --   | CAssign      CAssignOp               -- assignment operator
-- --                  (CExpression a)         -- l-value
-- --                  (CExpression a)         -- r-value
-- --                  a
-- --   | CCond        (CExpression a)         -- conditional
-- --                  (Maybe (CExpression a)) -- true-expression (GNU allows omitting)
-- --                  (CExpression a)         -- false-expression
-- --                  a
-- --   | CBinary      CBinaryOp               -- binary operator
-- --                  (CExpression a)         -- lhs
-- --                  (CExpression a)         -- rhs
-- --                  a
-- --   | CCast        (CDeclaration a)        -- type name
-- --                  (CExpression a)
-- --                  a
-- --   | CUnary       CUnaryOp                -- unary operator
-- --                  (CExpression a)
-- --                  a
-- --   | CSizeofExpr  (CExpression a)
-- --                  a
-- --   | CSizeofType  (CDeclaration a)        -- type name
-- --                  a
-- --   | CAlignofExpr (CExpression a)
-- --                  a
-- --   | CAlignofType (CDeclaration a)        -- type name
-- --                  a
-- --   | CComplexReal (CExpression a)         -- real part of complex number
-- --                  a
-- --   | CComplexImag (CExpression a)         -- imaginary part of complex number
-- --                  a
-- --   | CIndex       (CExpression a)         -- array
-- --                  (CExpression a)         -- index
-- --                  a
-- --   | CCall        (CExpression a)         -- function
-- --                  [CExpression a]         -- arguments
-- --                  a
-- --   | CMember      (CExpression a)         -- structure
-- --                  Ident                   -- member name
-- --                  Bool                    -- deref structure? (True for `->')
-- --                  a
-- --   | CVar         Ident                   -- identifier (incl. enumeration const)
-- --                  a
-- --   | CConst       (CConstant a)           -- ^ integer, character, floating point and string constants

-- transExpr :: CExpression a -> FC.Type
-- transExpr expr = case expr of
--     CConst _ (CConst _ _ _ (CTypeSepc CIntType _) val _) -> RFC.IntConst ((read val) :: Int)
--     CConst _ (CConst _ _ _ (CTypeSepc FPSingle _) val _) -> RFC.FPConst ((read val) :: Float)
--     CConst _ (CConst _ _ _ (CTypeSepc FPDouble _) val _) -> RFC.FPConst ((read val) :: Double)
--     CVar _ id ty -> Var (transType ty) id
--     CCast _  (CDecl [(CDeclr _ _ (Just t1) _ _),](CDeclr _ _ (Just t2) _ _) Nothing _) -> RFC.TypeCast (transType t1) (transType t2)
--     CCall _ f [arg] -> (translFunToUnaryOp f) (typeOfFun f) (transExpr arg)
--     CCall _ f [arg1 :: arg2] = (translFunToBinaryOp f) (typeOfFun f) (transExpr arg1) (transExpr arg2)
--     _ -> undefined

-- transBExpr :: CExpression a -> FC.Type
-- transBExpr expr = case expr of
--     CConst _ (CConst _ _ _ (CTypeSepc CBoolType _) "true" _) -> RFC.BTrue
--     CConst _ (CConst _ _ _ (CTypeSepc CBoolType _) "false" _) -> RFC.BFalse
--     CVar _ id ty -> Var (transType ty) id
--     CCast _  (CDecl [(CDeclr _ _ (Just t1) _ _),](CDeclr _ _ (Just t2) _ _) Nothing _) -> RFC.TypeCast (transType t1) (transType t2)
--     CCall _ f [arg] = (translFunToUnaryOp f) (typeOfFun f) (transExpr arg)
--     CCall _ f [arg1 :: arg2] = (translFunToBinaryOp f) (typeOfFun f) (transExpr arg1) (transExpr arg2)
--     _ -> undefined

-- transDecl :: CDeclaration a -> RFC.Stm
-- transDecl (CDecl [(CDeclr (Just id) _ (Just t) _ _)] Nothing _) = RFC.VarDecl (transType t) id
-- transDecl (CDecl [(CDeclr (Just id) _ (Just t) _ _)] aexpr _) = RFC.VarDecl (transType t) id transExpr

-- -- data CStatement a
  
-- --   = CLabel  Ident (CStatement a) [CAttribute a] a
-- --   | CCase (CExpression a) (CStatement a) a
-- --   | CCases (CExpression a) (CExpression a) (CStatement a) a
-- --   | CDefault (CStatement a) a
-- --   | CExpr (Maybe (CExpression a)) a
-- --   | CCompound [Ident] [CCompoundBlockItem a] a
-- --   | CIf (CExpression a) (CStatement a) (Maybe (CStatement a)) a
-- --   | CSwitch (CExpression a) (CStatement a) a
-- --   | CWhile (CExpression a) (CStatement a) Bool a
-- --   | CFor (Either (Maybe (CExpression a)) (CDeclaration a))
-- --     (Maybe (CExpression a))
-- --     (Maybe (CExpression a))
-- --     (CStatement a)
-- --     a
-- --   | CCont a
-- --   | CBreak a
-- --   | CReturn (Maybe (CExpression a)) a
-- --   | CAsm (CAssemblyStatement a) a
-- --     deriving (Show, Data,Typeable, Generic {-! , CNode , Annotated !-})

-- transStm :: CStatement a -> RFC.Stm
-- transStm cst = case cst of
--     CLabel _ _ [decl] _ -> transDecl decl
--     CCompound [id] [it] -> case (getType it) of
--         CBoolType _ -> RFC.VarAssignBool id (transBexpr it)
--         _           -> RFC.VarAssign (getType it) id (transExpr it)
--     CIf pred 'if 'else -> RFC.Ite transBexpr if' (map transStm 'if) (map transStm 'else)
--     CFor (Left (Just (expr)) id) (Just expr') stm -> RFC.ForLoop id (transExpr expr) (transExpr expr')
--     CReturn (Just expr) _ -> RFC.Return (transExpr expr)


-- transToReFlowC :: FilePath -> IO [RFC.Stm]
-- transToReFlowC = undefined


-- resultVar :: String
-- resultVar = "res"

-- args2C :: Arg -> C.Arg
-- args2C (Arg x fp) = C.Arg (fprec2type fp) x

-- args2CwithType :: C.Type -> Arg -> C.Arg
-- args2CwithType t (Arg x _) = C.Arg t x

-- resultNone ::  C.Type -> C.Stm
-- resultNone t = C.VarAssign t resultVar (C.None t)


-- removeValue :: C.AExpr -> C.AExpr
-- removeValue (C.Value expr) = expr
-- removeValue expr = expr

-- removeValueBExpr :: C.BExpr -> C.BExpr
-- removeValueBExpr (C.BValue expr) = expr
-- removeValueBExpr expr = expr

-- declareVar :: HasConditionals -> FAExpr -> Int -> C.Stm
-- declareVar hasConds expr@(FEFun _ f _ _ _) n
--   | hasConds f = C.VarDeclAssign (C.MaybeStruct (fprec2type $ getPVSType expr)) (auxVarName n)
--                  (removeValue $ aexpr2inlineC hasConds expr)
--   | otherwise = C.VarDeclAssign (fprec2type $ getPVSType expr) (auxVarName n)
--                  (aexpr2inlineC hasConds expr)

-- aexpr2C f hasConds forListExpr forMap t env args (Let listExpr body) = do
--   let (listVarAssign,env') = foldr (letElem2C hasConds args) ([],env) listExpr
--   bodyStm <- aexpr2C f hasConds forListExpr forMap t env' args body
--   return $ listVarAssign++bodyStm

-- aexpr2C f hasConds forListExpr forMap t env args (Ite bexpr thenExpr elseExpr) = do
--   let callList = funCallListFBExpr bexpr
--   callListVars <- generateAuxVarList callList
--   let predList = predCallListFBExpr bexpr
--   predListVars <- generateAuxVarList predList
--   thenStm  <- aexpr2C f hasConds forListExpr forMap t env args
--                      (replaceCallsInAExpr hasConds callListVars predListVars thenExpr)
--   elseStm  <- aexpr2C f hasConds forListExpr forMap t env args
--                      (replaceCallsInAExpr hasConds callListVars predListVars elseExpr)
--   return $ map (uncurry $ declareVar hasConds) callListVars
--            ++
--            map (uncurry $ declareBoolVar hasConds) predListVars
--            ++
--            [C.Ite (bexpr2C hasConds (replaceCallsInBExpr hasConds callListVars predListVars bexpr)) thenStm elseStm]




-- replaceInBExpr :: (AExpr -> Maybe AExpr) -> (BExpr -> Maybe BExpr) -> BExpr -> BExpr
-- replaceInBExpr aef bef expr = fromMaybe (replaceInBExpr' expr) (bef expr)
--   where
--     replaceInBExpr' (Or  be1 be2)     = Or  (replaceInBExpr aef bef be1) (replaceInBExpr aef bef be2)
--     replaceInBExpr' (And be1 be2)     = And (replaceInBExpr aef bef be1) (replaceInBExpr aef bef be2)
--     replaceInBExpr' (Not be)          = Not (replaceInBExpr aef bef be)
--     replaceInBExpr' (Rel op  ae1 ae2) = Rel op  (replaceInAExpr aef bef ae1) (replaceInAExpr aef bef ae2)
--     replaceInBExpr' (IsValid ae)      = IsValid  (replaceInAExpr aef bef ae)
--     replaceInBExpr' (BIsValid be)     = BIsValid (replaceInBExpr aef bef be)
--     replaceInBExpr' (FEPred f args)   = FEPred f (map (replaceInAExpr aef bef) args)
--     replaceInBExpr' BTrue  = BTrue
--     replaceInBExpr' BFalse = BFalse
--     replaceInBExpr' (BValue be) = BValue (replaceInBExpr aef bef be)
--     replaceInBExpr' (BVar t x) = BVar t x

-- replaceInAExpr :: (AExpr -> Maybe AExpr) -> (BExpr -> Maybe BExpr) -> AExpr -> AExpr
-- replaceInAExpr aef bef expr = fromMaybe (replaceInAExpr' expr) (aef expr)
--   where
--     replaceInAExpr' :: AExpr -> AExpr
--     replaceInAExpr' (TypeCast fromType toType ae) = TypeCast fromType toType (replaceInAExpr aef bef ae)
--     replaceInAExpr' (EFun f field t aes) = EFun f field t (map (replaceInAExpr aef bef) aes)
--     replaceInAExpr' (ArrayElem t v aes) = ArrayElem t v (map (replaceInAExpr aef bef) aes)
--     replaceInAExpr' (BinaryOp op t ae1 ae2) = BinaryOp  op t (replaceInAExpr aef bef ae1) (replaceInAExpr aef bef ae2)
--     replaceInAExpr' (UnaryOp  op t ae) = UnaryOp  op t (replaceInAExpr aef bef ae)
--     replaceInAExpr' (Min aes) = Min (map (replaceInAExpr aef bef) aes)
--     replaceInAExpr' (Max aes) = Max (map (replaceInAExpr aef bef) aes)
--     replaceInAExpr' (Value ae) = Value (replaceInAExpr aef bef ae)
--     replaceInAExpr' (IteExpr be thenExpr elseExpr) = IteExpr (replaceInBExpr aef bef be) (replaceInAExpr aef bef thenExpr) (replaceInAExpr aef bef elseExpr)
--     replaceInAExpr' (ForLoopExpr idx idxStart idxEnd body) = ForLoopExpr idx (replaceInAExpr aef bef idxStart) (replaceInAExpr aef bef idxEnd) (replaceInAExpr aef bef body)
--     replaceInAExpr' (Some t (Left  ae)) = Some t (Left  $ replaceInAExpr aef bef ae)
--     replaceInAExpr' (Some t (Right be)) = Some t (Right $ replaceInBExpr aef bef be)
--     replaceInAExpr' ae = ae

-- getValueFunCallsBExpr :: BExpr -> BExpr
-- getValueFunCallsBExpr (Not be) = Not (getValueFunCallsBExpr be)
-- getValueFunCallsBExpr (Or  be1 be2) = Or  (getValueFunCallsBExpr be1) (getValueFunCallsBExpr be2)
-- getValueFunCallsBExpr (And be1 be2) = And (getValueFunCallsBExpr be1) (getValueFunCallsBExpr be2)
-- getValueFunCallsBExpr (Rel Eq  ae1 ae2) = Rel Eq  (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
-- getValueFunCallsBExpr (Rel Neq ae1 ae2) = Rel Neq (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
-- getValueFunCallsBExpr (Rel Lt  ae1 ae2) = Rel Lt  (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
-- getValueFunCallsBExpr (Rel LtE ae1 ae2) = Rel LtE (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
-- getValueFunCallsBExpr (Rel Gt  ae1 ae2) = Rel Gt  (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
-- getValueFunCallsBExpr (Rel GtE ae1 ae2) = Rel GtE (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
-- getValueFunCallsBExpr be@(FEPred _ _) = BValue be
-- getValueFunCallsBExpr be =  be

-- getValueFunCallsAExpr :: AExpr -> AExpr
-- getValueFunCallsAExpr = replaceInAExpr getValueFunCallAExpr (Just . getValueFunCallsBExpr)
--   where
--     getValueFunCallAExpr fun@EFun{} = Just $ Value fun
--     getValueFunCallAExpr _ = Nothing
