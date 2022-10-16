{-# LANGUAGE BlockArguments #-}

module Dep.Logger.HandlerAware (make) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Dep.Logger
import Servant.Server.HandlerContext

make :: (MonadIO m, MonadReader env m, HasHandlerContext env) => Logger m
make = Logger \message -> do
  context <- view handlerContext
  liftIO $ putStrLn $ show context ++ " " ++ message
  pure ()