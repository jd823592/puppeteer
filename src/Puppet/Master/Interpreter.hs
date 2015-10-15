module Puppet.Master.Interpreter ( InterpreterWorker
                                 , newInterpreterWorker
                                 , ask
                                 , eval
                                 , typeOf
                                 ) where

import Control.Monad

import qualified Language.Haskell.Interpreter as I

import Puppet.Master.Worker

type InterpreterWorker = Worker I.Interpreter

eval :: String -> I.Interpreter String
eval = I.eval

typeOf :: String -> I.Interpreter String
typeOf = I.typeOf

newInterpreterWorker :: IO InterpreterWorker
newInterpreterWorker = newWorker $ I.runInterpreter
                                 . (I.setImports [ "Prelude" ] >>) >=> print
