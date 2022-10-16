{-# LANGUAGE BlockArguments #-}

-- A Logger implementation that is aware of the current Servant handler.
module Dep.Logger.HandlerAware (make) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Dep.Logger
import Servant.Server.HandlerContext

-- | Notice that *none* of the components in @Counter.Model@ has a
-- @HasHandlerContext@ constraint, despite many of them using the 'Logger'
-- component.
--
-- The need to know about @HasHandlerContext@ is contained in the 'Logger'
-- implementation.
make ::
  ( MonadIO m,
    MonadReader env m,
    HasHandlerContext env
  ) =>
  Logger m
make = Logger \message -> do
  context <- view handlerContext
  liftIO $ putStrLn $ show (reverse context) ++ " " ++ message
  pure ()