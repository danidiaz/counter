{-# LANGUAGE BlockArguments #-}
module Dep.Logger.HandlerAware where

import Servant.Server.HandlerContext
import Control.Monad.IO.Class
import Dep.Logger
import Control.Monad.Reader
import Control.Lens

make :: (MonadIO m, MonadReader env m, HasHandlerContext env) => Logger m 
make = Logger \message -> do
    context <- view handlerContext 
    liftIO $ putStrLn $ show context ++ ". " ++ message
    pure ()