module Types where

import AbsKlatte
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M

type VEnv = M.Map Ident Loc
type FEnv = M.Map Ident Loc
type Env = (FEnv, VEnv)

type Store = (FStore, VStore, FDeclCnt, VDeclCnt)
type FStore = M.Map Loc FunInfo
type VStore = M.Map Loc Value

-- counters for how many vars/funs were declarated
type FDeclCnt = Int
type VDeclCnt = Int

data FunInfo = FunInfo Env Type [Arg] Block deriving Show
data Value = IntVal Integer | BoolVal Bool | VoidVal () deriving (Ord, Eq)
instance Show Value where
    show (IntVal x) = "int " ++ show x
    show (BoolVal x) = "bool " ++ show x
    show (VoidVal x) = "void"

data ReturnValue = NotReturn | RetVal Value deriving (Show, Ord, Eq)
newtype Loc = Loc Int deriving (Show, Ord, Eq)

type InterMonad a = ReaderT Env (ErrorT String (StateT Store IO)) a

