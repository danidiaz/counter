{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- A Logger implementation that is aware of the current Servant handler.
module Dep.Logger.HandlerAware (Conf(..), make, alloc, Manager (..)) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Dep.Logger
import Servant.Server.HandlerContext
import Data.Aeson ((.:), FromJSON (..), Value(..), withObject)
import GHC.Generics (Generic)
import Data.IORef

newtype Conf = Conf { 
        minimumLevel :: LogLevel
    } deriving stock (Show, Generic)

instance FromJSON Conf where
  parseJSON = withObject "Conf" \o -> do
    levelValue <- o .: "minimumLevel"
    minimumLevel <- case levelValue of 
          String "trace" -> pure Trace
          String "debug" -> pure Debug
          String "info" -> pure Info
          String "warn" -> pure Warn
          String "error" -> pure Error
          String "fatal" -> pure Fatal
          _ -> fail "unknown level"
    pure $ Conf {minimumLevel}

data Manager m = Manager {
    resetLogLevel :: m (),
    setLogLevel :: LogLevel -> m (),
    logger :: Logger m
  }

type State = IORef LogLevel

alloc :: MonadIO m => Conf -> m State
alloc Conf {minimumLevel} = liftIO $ newIORef minimumLevel

-- | Notice that *none* of the components in @Counter.Model@ has a
-- @HasHandlerContext@ constraint, despite many of them using the 'Logger'
-- component.
--
-- The need to know about @HasHandlerContext@ is contained in the 'Logger'
-- implementation.
make ::
  ( MonadIO m,
    MonadReader renv m,
    HasHandlerContext renv
  ) =>
  Conf ->
  State ->
  -- | not used, but for consistency with other components
  env ->
  Manager m
make Conf {minimumLevel} ref _ = Manager {
  resetLogLevel = liftIO $ writeIORef ref minimumLevel,
  setLogLevel = \newLevel -> liftIO $ writeIORef ref newLevel,
  logger = Logger \level message -> do
    currentLevel <- liftIO $ readIORef ref
    when (level >= currentLevel) do
      context <- view handlerContext
      liftIO $ putStrLn $ show (reverse context) ++ " " ++ message
      pure ()
}