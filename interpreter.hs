module Main where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import System.IO
import System.Environment
import LexKlatte
import ParKlatte
import AbsKlatte
import InterpreterRun
import Types
import Utils
import ErrM

main :: IO ()
main = do
  args <- getArgs
  code <- if null args
       then getContents
       else readFile $ head args
  case pProgram (myLexer code) of
       Ok p   -> do
            (value, (fStore, vStore, _, _)) <- runInterMonad emptyEnv emptyStore (interpretRun p)
            case value of
                Left msg         -> hPutStrLn stderr ("Runtime error: " ++ msg)
                Right (IntVal v) -> if v >= 0
                    then putStrLn "Successfully interpreted program!"
                    else putStrLn "Main returned error code!"
       Bad err -> hPutStrLn stderr ("Parse error: " ++ err)

