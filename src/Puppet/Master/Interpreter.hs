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
import GhcMonad
import Outputable

import Puppet.Master.Worker

type InterpreterWorker = Worker Ghc

eval :: String -> Ghc String
eval e = attempt >>= return . either (const  "error") (showSDocUnsafe . ppr) where
    attempt :: Ghc (Either SomeException [Name])
    attempt = gtry $ runStmt e RunToCompletion >>= result

    result :: RunResult -> Ghc [Name]
    result (RunOk ns) = return ns
    result _          = return []

typeOf :: String -> Ghc String
typeOf e = attempt >>= return . either (const "no type") (showSDocUnsafe . ppr) where
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
