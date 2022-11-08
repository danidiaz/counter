{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- A Logger implementation that is aware of the current Servant handler.
module Dep.Logger.HandlerAware (Conf (..), make, alloc) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, (.:))
import Data.Aeson.Types (object)
import Data.IORef
import Dep.Has
import Dep.Has.Call
import Dep.Knob
import Dep.Knob.IORef qualified
import Dep.Logger
import GHC.Generics (Generic)
import Servant.Server.HandlerContext

newtype Conf = Conf
  { minimumLevel :: LogLevel
  }
  deriving stock (Show, Generic)

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

type Refs = IORef Conf

alloc :: MonadIO m => Conf -> m Refs
alloc conf = liftIO $ newIORef conf

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
  Refs ->
  (Knob Conf m, Logger m)
make conf ref =
  ( Dep.Knob.IORef.make conf ref,
    Logger \level message -> do
      Conf {minimumLevel} <- liftIO $ readIORef ref
      when (level >= minimumLevel) do
        context <- view handlerContext
        liftIO $ putStrLn $ show (reverse context) ++ " " ++ message
        pure ()
  )
