module Utils where

import Types
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M
import AbsKlatte

runInterMonad :: Env -> Store -> InterMonad a -> IO (Either String a, Store)
runInterMonad env st im = runStateT (runErrorT (runReaderT im env)) st

emptyEnv :: Env
emptyEnv = (M.empty, M.empty)

emptyStore :: Store
emptyStore = (M.empty, M.empty, 0, 0)