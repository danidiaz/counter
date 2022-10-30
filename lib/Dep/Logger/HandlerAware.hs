{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- A Logger implementation that is aware of the current Servant handler.
module Dep.Logger.HandlerAware (Conf(..), make, alloc, LoggerKnob, unknob) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Dep.Logger
import Dep.Has
import Dep.Has.Call
import Dep.Knob
import Servant.Server.HandlerContext
import Data.Aeson ((.:), ToJSON (..), FromJSON (..), Value(..), withObject)
import GHC.Generics (Generic)
import Data.IORef
import Data.Aeson.Types (object)

newtype Conf = Conf { 
        minimumLevel :: LogLevel
    } deriving stock (Show, Generic)

-- Repeating the names of each branch in FromJSON and ToJSON is awful,
-- but let's go with it for now.
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

instance ToJSON Conf where
  toJSON (Conf {minimumLevel}) = 
    let levelString = case minimumLevel of
          Trace -> "trace"
          Debug -> "debug"
          Info -> "info"
          Warn -> "warn"
          Error -> "error"
          Fatal -> "fatal"
     in object [("minimumLevel", String levelString)]

type State = IORef Conf

alloc :: MonadIO m => Conf -> m State
alloc conf  = liftIO $ newIORef conf

type LoggerKnob = Knob Conf Logger

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
  Conf ->
  State ->
  -- | not used, but for consistency with other components
  deps ->
  LoggerKnob m
make conf ref _ = Knob {
  resetKnob = liftIO $ writeIORef ref conf,
  setKnob = \newConf -> liftIO $ writeIORef ref newConf,
  inspectKnob = liftIO $ readIORef ref,
  knobComponent = Logger \level message -> do
    Conf {minimumLevel} <- liftIO $ readIORef ref
    when (level >= minimumLevel) do
      context <- view handlerContext
      liftIO $ putStrLn $ show (reverse context) ++ " " ++ message
      pure ()
}

-- | Extract the inner Logger from the LoggerKnob
unknob :: Has LoggerKnob m deps => deps -> Logger m
unknob = knobComponent @Conf @Logger . dep