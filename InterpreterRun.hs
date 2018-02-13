module InterpreterRun where

import AbsKlatte
import Types
import Utils
import Memory
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import System.IO
import System.Environment
import qualified Data.Map as M
import Data.List

-- info parameter keeps the information that is shown when error occurs

-------------------------------- General Expr ----------------------------------

evalGeneralExpr :: GeneralExpr -> InterMonad Value

evalGeneralExpr (NExpr e) = evalExpr (show e) e

evalGeneralExpr (LExpr e) = do
    evalLamExpr e
    return $ VoidVal ()

------------------------------------- Expr -------------------------------------

evalExpr :: String -> Expr -> InterMonad Value

evalExpr info (EVar id) = do { getVar info id; }

evalExpr info (ELitInt n) = return $ IntVal n

evalExpr info ELitTrue = return $ BoolVal True

evalExpr info ELitFalse = return $ BoolVal False

evalExpr info (Neg e) = do
    IntVal val <- evalIntExpr info e
    return $ IntVal (-val)

evalExpr info (Not e) = do
    BoolVal val <- evalBoolExpr info e
    return $ BoolVal (not val)

evalExpr info (EMul e1 op e2) = do
    IntVal val1 <- evalIntExpr info e1
    IntVal val2 <- evalIntExpr info e2
    case op of
        Times -> return $ IntVal (val1 * val2)
        Div   -> if val2 /= 0
                    then return $ IntVal (val1 `div` val2)
                    else throwError $ "Division by 0 in: " ++ info
        Mod   -> return $ IntVal (val1 `mod` val2)

evalExpr info (EAdd e1 op e2) = do
    IntVal val1 <- evalIntExpr info e1
    IntVal val2 <- evalIntExpr info e2
    case op of
        Plus  -> return $ IntVal (val1 + val2)
        Minus -> return $ IntVal (val1 - val2)

evalExpr info (ERel e1 op e2) = do
    val1 <- evalExpr info e1
    val2 <- evalExpr info e2
    case op of
        LTH -> return $ BoolVal $ if val1 <  val2 then True else False
        LE  -> return $ BoolVal $ if val1 <= val2 then True else False
        GTH -> return $ BoolVal $ if val1 >  val2 then True else False
        GE  -> return $ BoolVal $ if val1 >= val2 then True else False
        EQU -> return $ BoolVal $ if val1 == val2 then True else False
        NE  -> return $ BoolVal $ if val1 /= val2 then True else False

evalExpr info (EAnd e1 e2) = do
    BoolVal val1 <- evalBoolExpr info e1
    if val1 == False
        then return $ BoolVal False
        else do
            BoolVal val2 <- evalBoolExpr info e2
            return $ BoolVal $ val1 && val2

evalExpr info (EOr e1 e2) = do
    BoolVal val1 <- evalBoolExpr info e1
    if val1 == True
        then return $ BoolVal True
        else do
            BoolVal val2 <- evalBoolExpr info e2
            return $ BoolVal $ val1 || val2

evalExpr info eApp@(EApp id@(Ident name) exprs) = do
    env <- ask
    FunInfo envF rType args block <- getFun (show eApp) id
    (_, _, fDCnt, vDCnt) <- get
    env' <- local (const envF) (interpretArgs (show eApp) env args exprs)
    rVal <- local (const env') (interpretBlock block)
    deleteUnnecessaryFun fDCnt
    deleteUnnecessaryVar vDCnt
    checkReturnType (show eApp) rVal rType

evalExpr info lApp@(LApp (ELam rType args block) exprs) = do
    env <- ask
    (_, _, fDCnt, vDCnt) <- get
    env' <- interpretArgs (show lApp) env args exprs
    rVal <- local (const env') (interpretBlock block)
    deleteUnnecessaryFun fDCnt
    deleteUnnecessaryVar vDCnt
    checkReturnType (show lApp) rVal rType

evalBoolExpr :: String -> Expr -> InterMonad Value
evalBoolExpr info e = do
    val <- evalExpr info e
    case val of
        BoolVal v -> return val
        _         -> throwError $ "Type error: bool value expected in: " ++ info

evalIntExpr :: String -> Expr -> InterMonad Value
evalIntExpr info e = do
    val <- evalExpr info e
    case val of
        IntVal v -> return val
        _        -> throwError $ "Type error: integer value expected in: " ++ info

---------------------------------- Lambda Expr ---------------------------------

evalLamExpr :: LamExpr -> InterMonad FunInfo
evalLamExpr (ELam rType args block) = do
    env <- ask
    return $ (FunInfo env rType args block)

------------------------------------ Block -------------------------------------

interpretBlock :: Block -> InterMonad ReturnValue
interpretBlock (Block b) = do
    env <- ask
    (_, _, _, vDCnt) <- get
    local (const env) (iterateBlock vDCnt b)

iterateBlock :: Int -> [Stmt] -> InterMonad ReturnValue
iterateBlock entVDecCnt [] = return NotReturn
iterateBlock entVDecCnt (s:t) = do
    case s of
        Decl typ items -> do
            env <- interpretDecl (show s) entVDecCnt typ items
            local (const env) (iterateBlock entVDecCnt t)
        _              -> do
            val <- interpretStmt s
            case val of
                NotReturn -> iterateBlock entVDecCnt t
                _         -> return val

------------------------------------- Stmt -------------------------------------

interpretStmt :: Stmt -> InterMonad ReturnValue
interpretStmt (BStmt b) = do
    env <- ask
    (_, _, fDCnt, vDCnt) <- get
    rVal <- local (const env) (interpretBlock b)
    deleteUnnecessaryFun fDCnt
    deleteUnnecessaryVar vDCnt
    return rVal

interpretStmt Empty = return NotReturn

interpretStmt stmt@(Ass id e) = do
    loc <- getVarLoc (show stmt) id
    var <- getVar (show stmt) id
    val' <- case var of
        IntVal val -> evalIntExpr (show stmt) e
        BoolVal val -> evalBoolExpr (show stmt) e
    storeVar val' loc
    return NotReturn

interpretStmt stmt@(Ret e) = do
    val <- evalExpr (show stmt) e
    return $ RetVal val

interpretStmt VRet = return $ RetVal $ VoidVal ()

interpretStmt stmt@(Cond e s) = do
    BoolVal val <- evalBoolExpr (show stmt) e
    if val == True
        then interpretStmt s
        else return NotReturn

interpretStmt stmt@(CondElse e s1 s2) = do
    BoolVal val <- evalBoolExpr (show stmt) e
    if val == True
        then interpretStmt s1
        else interpretStmt s2

interpretStmt stmt@(While e s) = do
    BoolVal val <- evalBoolExpr (show stmt) e
    if val == True
        then do
            rVal <- interpretStmt s
            case rVal of
                NotReturn -> interpretStmt (While e s)
                _         -> return rVal
        else return NotReturn

interpretStmt stmt@(ForTo typ id begE endE block) = do
    (_, _, _, vDCnt) <- get
    env <- interpretDecl (show stmt) vDCnt typ [(Init id begE)]
    local (const env) (runFor "to" id endE block)

interpretStmt stmt@(ForDownTo typ id begE endE block) = do
    (_, _, _, vDCnt) <- get
    env <- interpretDecl (show stmt) vDCnt typ [(Init id begE)]
    local (const env) (runFor "downto" id endE block)

interpretStmt stmt@(Print e) = do
    val <- evalExpr (show stmt) e
    case val of
        IntVal v  -> liftIO $ print v
        BoolVal v -> liftIO $ print v
        _         -> throwError $ "Expression cannot be printed: " ++ show e
    return NotReturn

interpretStmt stmt@(SExp e) = do 
    val <- evalExpr (show stmt) e
    return NotReturn

------------------------------------ Utils -------------------------------------

---------------------------------- For utils -----------------------------------

runFor :: String -> Ident -> Expr -> Block -> InterMonad ReturnValue
runFor forType id endE block = do
    let info = "for " ++ show id ++ " ... " ++ forType ++ 
               " " ++ show endE ++ " " ++ show block
    IntVal endVal <- evalIntExpr info endE
    IntVal idVal <- getVar info id
    if (endVal < idVal && forType == "downto") || (endVal > idVal && forType == "to")
        then do
            (_, _, fDCnt, vDCnt) <- get
            rVal <- interpretBlock block
            deleteUnnecessaryFun fDCnt
            deleteUnnecessaryVar vDCnt
            store <- get
            case rVal of
                NotReturn -> do
                    loc <- getVarLoc info id
                    IntVal idVal <- getVar info id
                    if forType == "to"
                        then do
                            storeVar (IntVal (idVal + 1)) loc
                            runFor "to" id endE block
                        else do
                            storeVar (IntVal (idVal - 1)) loc
                            runFor "downto" id endE block
                _        -> return rVal
        else return NotReturn

------------------------------- Declaration utils ------------------------------

-- entVDecCnt - it shows from which location starts new scope
interpretDecl :: String -> Int -> Type -> [Item] -> InterMonad Env
interpretDecl info entVDecCnt typ [] = do { env <- ask; return env; }
interpretDecl info entVDecCnt typ ((Init id e):items) = do
    env <- ask
    store <- get
    (result, _) <- liftIO $ runInterMonad env store (getVarLoc info id)
    case result of
        Right (Loc l) -> if entVDecCnt >= l
                            then continueDecl
                            else throwError $ "Redeclaration of variable " ++ 
                                 show id ++ " in: " ++ info
        Left msg      -> continueDecl
    where continueDecl = do
            val <- evalExpr info e
            env' <- if (checkVarType typ val) || (checkVarType typ val)
                        then declVar id val
                        else throwError $ "Declaration type doesn't match " ++
                             "assigned type. Variable " ++ show id ++
                             " in: " ++ info
            local (const env') (interpretDecl info entVDecCnt typ items)

declVar :: Ident -> Value -> InterMonad Env
declVar id val = do
    env <- assignVarNewLoc id
    loc <- local (const env) (getVarLoc (show val) id)
    storeVar val loc
    return env

-------------------------------- Arguments utils -------------------------------

-- evokedEnv is an environment in which function has been evoked
-- InterMonad keeps environment which is stored by evoking function
-- interpretArgs returns function environment updated with arguments
interpretArgs :: String -> Env -> [Arg] -> [GeneralExpr] -> InterMonad Env
interpretArgs info evokedEnv [] (e:et) =
    throwError $ "Incorrect number of arguments in function call: " ++ info
interpretArgs info evokedEnv (a:at) [] =
    throwError $ "Incorrect number of arguments in function call: " ++ info
interpretArgs info evokedEnv [] [] = do { env <- ask; return env; }
interpretArgs info evokedEnv args@(arg:at) (e:et) = do
    if checkArgsNames args == True
        then do 
            env <- case e of
                LExpr e' -> storeLamArg info arg e'
                NExpr e' -> storeArg info evokedEnv arg e'
            local (const env) (interpretArgs info evokedEnv at et)
        else throwError $ "Repeated argument names. Function: " ++ info

storeArg :: String -> Env -> Arg -> Expr -> InterMonad Env
storeArg info evokedEnv (ValArg typ id) e = do
    val <- local (const evokedEnv) (evalExpr info e)
    case checkVarType typ val of
        True  -> declVar id val
        False -> throwError $ "Argument " ++ show id ++ " - type doesn't match. " ++
                              "Function call: " ++ info

storeArg info evokedEnv arg@(VarArg typ id) e = do
    case e of
        EVar x -> do
            val <- local (const evokedEnv) (getVar info x)
            case checkVarType typ val of
                True  -> do
                    loc <- local (const evokedEnv) (getVarLoc info x)
                    assignVarLoc id loc
                False -> throwError $ "Reference argument: " ++ show arg
                                      ++ "  - type doesn't match. "
                                      ++ "Function call: " ++ info
        _      -> throwError $ "Expected named argument. Argument: " ++ show arg ++
                               " in function call: " ++ info

storeArg info evokedEnv (FunArg (Fun rType argTypes) id) e = do
    case e of
        EVar evokedFId -> do
            FunInfo env rFType args b <- local (const evokedEnv) (getFun info evokedFId)
            if rType == rFType
                then do
                    checkFunArgTypes info args argTypes
                    loc <- local (const evokedEnv) (getFunLoc info evokedFId)
                    assignFunLoc id loc
                else throwError $ "Return type of functional argument doesn't match. "
                                  ++ "Argument: " ++ show id ++ ". Function call: " ++ info
        _      -> throwError $ "Expected functional argument " ++ show id ++
                               "Function call: " ++ info

storeLamArg :: String -> Arg -> LamExpr -> InterMonad Env
storeLamArg info (FunArg (Fun rType argTypes) id@(Ident name)) e = do
    lamInfo@(FunInfo envLam rTypeLam argsLam bLam) <- evalLamExpr e
    if rType == rTypeLam
        then do
            checkFunArgTypes info argsLam argTypes
            env <- assignFunNewLoc id
            loc <- local (const env) (getFunLoc info id)
            storeFunInfo lamInfo loc
            return env
        else throwError $ "Return type of functional argument doesn't match. "
                           ++ "Argument: " ++ name ++ ". Function call: " ++ info

----------------------------- Deleting vars/funs -------------------------------

deleteUnnecessaryFun :: FDeclCnt -> InterMonad ()
deleteUnnecessaryFun prevFDCnt = do
    (fStore, vStore, fDCnt, vDCnt) <- get
    let lastLoc = M.size fStore
        fStore' = deleteLoc [(prevFDCnt + 1) .. lastLoc] fStore
    put (fStore', vStore, prevFDCnt, vDCnt)

deleteUnnecessaryVar :: VDeclCnt -> InterMonad ()
deleteUnnecessaryVar prevVDCnt = do
    (fStore, vStore, fDCnt, vDCnt) <- get
    let lastLoc = M.size vStore
        vStore' = deleteLoc [(prevVDCnt + 1) .. lastLoc] vStore
    put (fStore, vStore', fDCnt, prevVDCnt)

--------------------------------- Type checking --------------------------------

-- check whether argument names are unique
checkArgsNames :: [Arg] -> Bool
checkArgsNames args = 
    let names = map getArgName args
    in (length names) == (length $ nub names)

getArgName :: Arg -> String
getArgName (VarArg _ (Ident name)) = name
getArgName (ValArg _ (Ident name)) = name
getArgName (FunArg _ (Ident name)) = name

checkVarType :: Type -> Value -> Bool
checkVarType typ val = case (typ, val) of
    (Int, IntVal _)   -> True
    (Bool, BoolVal _) -> True
    (Void, VoidVal _) -> True
    _                 -> False

checkFunArgTypes :: String -> [Arg] -> [FunPartType] -> InterMonad Bool
checkFunArgTypes info [] (h:t) = throwError $ "Incorrect number of arguments " ++
        "in functional argument. Function call: " ++ info
checkFunArgTypes info (h:t) [] = throwError $ "Incorrect number of arguments " ++
        "in functional argument. Function call: " ++ info
checkFunArgTypes info [] [] = return True
checkFunArgTypes info (arg:args) (typ:types) = do
    case (arg, typ) of
        (ValArg _ _, ValType _) -> checkFunArgTypes info args types
        (VarArg _ _, VarType _) -> checkFunArgTypes info args types
        (FunArg _ _, FType _)   -> checkFunArgTypes info args types
        _                       -> throwError $ "Argument types mismatch "
                                    ++ "in functional argument: " ++ show arg
                                    ++ " <> " ++ show typ
                                    ++ ". Function call: " ++ info

checkReturnType :: String -> ReturnValue -> Type -> InterMonad Value
checkReturnType info rVal rType = do
    case (rVal, rType) of
        (NotReturn, Void) -> return $ VoidVal ()
        (RetVal val, _)   -> case checkVarType rType val of
            True  -> return val
            False -> throwError $ "Function return type doesn't match. " ++
                                "Function call: " ++ info
        _                 -> throwError $ "Function return type doesn't match. " ++
                                "Function call: " ++ info

----------------------------- Global vars/funs ---------------------------------

interpretTopDef :: TopDef -> InterMonad Env
interpretTopDef fun@(FnDef t id args b) = do
    if checkArgsNames args == True
        then do
            env <- ask
            store <- get
            (result, _) <- liftIO $ runInterMonad env store (getFunLoc (show fun) id)
            case result of
                Right l -> throwError $ "Function " ++ show id 
                                        ++ " already declared in: " ++ show fun
                Left e  -> do
                    env' <- assignFunNewLoc id
                    loc <- local (const env') (getFunLoc (show fun) id)
                    local (const env') (storeFun fun loc)
                    return env'
        else throwError $ "Repeated argument names. Function: " ++ show fun

interpretTopDef var@(VarDef typ items) = do
    interpretDecl (show var) 0 typ items

interpretProgram :: [TopDef] -> InterMonad Env
interpretProgram [] = do { env <- ask; return env; }
interpretProgram (fun:t) = do
    env <- interpretTopDef fun
    local (const env) (interpretProgram t)

interpretRun :: Program -> InterMonad Value
interpretRun (Program funs) = do
    env <- interpretProgram funs
    local (const env) (evalExpr "" (EApp (Ident "main") []))







