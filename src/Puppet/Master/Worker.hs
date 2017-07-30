module Puppet.Master.Worker where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

newtype Worker m = Worker { requests :: Chan (m ()) }

newWorker :: MonadIO m => (m () -> IO ()) -> IO (Worker m)
newWorker unlift = do
    channel <- newChan
    let loop = forever . join . liftIO $ readChan channel
    forkIO $ unlift loop
    return $ Worker channel

ask :: MonadIO m => Worker m -> m a -> IO a
ask w a = do
    ref <- newEmptyMVar
    writeChan (requests w) $ liftIO . putMVar ref =<< a
    takeMVar ref
