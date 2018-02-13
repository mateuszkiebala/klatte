module Memory where

import Types
import Utils
import AbsKlatte
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M

-- find next available location for fun/var
nextLoc :: M.Map Loc a -> Loc
nextLoc store = Loc $ M.size store + 1

-- delete location from environment
deleteLoc :: [Int] -> M.Map Loc a -> M.Map Loc a
deleteLoc [] store = store
deleteLoc (loc:locs) store = 
    deleteLoc locs (M.delete (Loc loc) store)

--------------------------- FUNCTIONS ENVIRONMENT ------------------------------

assignFunNewLoc :: Ident -> InterMonad Env
assignFunNewLoc id = do
    (fEnv, vEnv) <- ask
    (fStore, _, _, _) <- get
    let loc = nextLoc fStore
    return $ (M.insert id loc fEnv, vEnv)

assignFunLoc :: Ident -> Loc -> InterMonad Env
assignFunLoc id loc = do
    (fEnv, vEnv) <- ask
    return $ (M.insert id loc fEnv, vEnv)

getFunLoc :: String -> Ident -> InterMonad Loc
getFunLoc info id = do
    (fEnv, _) <- ask
    case M.lookup id fEnv of
        Just l  -> return l
        Nothing -> throwError $ "Function " ++ show id ++ " is not declared in: "
                                ++ info

------------------------------ FUNCTIONS STORE ---------------------------------

storeFun :: TopDef -> Loc -> InterMonad ()
storeFun (FnDef rType id args block) loc = do
    env <- ask
    storeFunInfo (FunInfo env rType args block) loc

storeFunInfo :: FunInfo -> Loc -> InterMonad ()
storeFunInfo funInfo loc = do
    (fStore, vStore, fDCnt, vDCnt) <- get
    case M.lookup loc fStore of
        Just l  -> put (M.insert loc funInfo fStore, vStore, fDCnt, vDCnt)
        Nothing -> put (M.insert loc funInfo fStore, vStore, fDCnt + 1, vDCnt)

getFun :: String -> Ident -> InterMonad FunInfo
getFun info id = do
    (fStore, _, _, _) <- get
    loc <- getFunLoc info id
    case M.lookup loc fStore of
        Just fInfo -> return fInfo
        Nothing    -> throwError $ "Unbound function: " ++ show id ++ ". " ++ info


--------------------------- VARIABLES ENVIRONMENT ------------------------------

assignVarNewLoc :: Ident -> InterMonad Env
assignVarNewLoc id = do
    (fEnv, vEnv) <- ask
    (_, vStore, _, _)  <- get
    let loc = nextLoc vStore
    return $ (fEnv, M.insert id loc vEnv)

assignVarLoc :: Ident -> Loc -> InterMonad Env
assignVarLoc id loc = do
    (fEnv, vEnv) <- ask
    return $ (fEnv, M.insert id loc vEnv)

getVarLoc :: String -> Ident -> InterMonad Loc
getVarLoc info id = do
    (_, vEnv) <- ask
    case M.lookup id vEnv of
        Just l  -> return l
        Nothing -> throwError $ "Variable " ++ show id ++ " is not declared in: "
                                ++ info

------------------------------ VARIABLES STORE ---------------------------------

storeVar :: Value -> Loc -> InterMonad ()
storeVar val loc = do
    (fStore, vStore, fDCnt, vDCnt) <- get
    case M.lookup loc vStore of
        Just l  -> put (fStore, (M.insert loc val vStore), fDCnt, vDCnt)
        Nothing -> put (fStore, (M.insert loc val vStore), fDCnt, vDCnt + 1)

getVar :: String -> Ident -> InterMonad Value
getVar info id = do
    (_, vStore, _, _) <- get
    loc <- getVarLoc info id
    case M.lookup loc vStore of
        Just vInfo -> return vInfo
        Nothing    -> throwError $ "Unbound variable: " ++ show id ++ ". " ++ info

