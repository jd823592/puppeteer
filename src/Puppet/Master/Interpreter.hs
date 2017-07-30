module Puppet.Master.Interpreter ( InterpreterWorker
                                 , newInterpreterWorker
                                 , ask
                                 , eval
                                 , typeOf
                                 ) where

import Exception
import Control.Monad

import GHC
import GHC.Paths ( libdir )
import GHCi
import GhcMonad
import Outputable

import Puppet.Master.Worker

type InterpreterWorker = Worker Ghc

eval :: String -> Ghc String
eval e = liftM (either (const  "error") (showSDocUnsafe . ppr)) attempt where
    attempt :: Ghc (Either SomeException [Name])
    attempt = gtry $ execStmt e (ExecOptions RunToCompletion "prompr" 0 EvalThis) >>= result

    result :: ExecResult -> Ghc [Name]
    result (ExecComplete (Right ns) _) = return ns
    result _                           = return []

typeOf :: String -> Ghc String
typeOf e = liftM (either (const "") (showSDocUnsafe . ppr)) attempt where
    attempt :: Ghc (Either SomeException Type)
    attempt = gtry $ exprType e

initGhc :: Ghc ()
initGhc = do
    df <- getSessionDynFlags
    setSessionDynFlags $ df { hscTarget = HscInterpreted
                            , ghcLink   = LinkInMemory
                            }
    setContext $ map (IIDecl . simpleImportDecl . mkModuleName) [ "Prelude" ]
    return ()

newInterpreterWorker :: IO InterpreterWorker
newInterpreterWorker = newWorker $ runGhc (Just libdir) . (initGhc >>) >=> const (return ())
