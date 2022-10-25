{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- A Logger implementation that is aware of the current Servant handler.
module Dep.Logger.HandlerAware (Conf(..), make) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Dep.Logger
import Servant.Server.HandlerContext
import Data.Aeson ((.:), FromJSON (..), Value(..), withObject)
import GHC.Generics (Generic)

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
  -- | not used, but for consistency with other components
  env ->
  Logger m
make Conf {minimumLevel} _ = Logger \level message -> do
  when (level >= minimumLevel) do
    context <- view handlerContext
    liftIO $ putStrLn $ show (reverse context) ++ " " ++ message
    pure ()