{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- A Logger implementation that is aware of the current Servant handler.
module Dep.Logger.HandlerAware (Conf (..), make, alloc) where

import ByOtherNames
import ByOtherNames.Aeson (GeneralJSONEnum (..))
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, (.:))
import Data.Aeson.Types (Key, object)
import Data.IORef
import Data.Typeable (typeRepTyCon)
import Dep.Has
import Dep.Has.Call
import Dep.Knob
import Dep.Knob.IORef qualified
import Dep.Logger
import GHC.Generics (Generic)
import Servant.Server.HandlerContext
import Prelude hiding (log)

newtype Conf = Conf
  { minimumLevel :: ConfLogLevel
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype ConfLogLevel = ConfLogLevel {confLevel :: LogLevel}
  deriving (Show)
  deriving (FromJSON, ToJSON) via GeneralJSONEnum JSONLocal LogLevel

data JSONLocal

instance Rubric JSONLocal where
  type AliasType JSONLocal = Key

instance Aliased JSONLocal LogLevel where
  aliases =
    aliasListBegin 
    $ alias @"Trace" "trace" 
    $ alias @"Debug" "debug" 
    $ alias @"Info" "info" 
    $ alias @"Warn" "warn" 
    $ alias @"Error" "error" 
    $ alias @"Fatal" "fatal" 
    $ aliasListEnd

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
  m Conf ->
  Logger m
make askConf = do
  let logFor = \mrep level message -> do
        Conf {minimumLevel = ConfLogLevel {confLevel}} <- askConf
        when (level >= confLevel) do
          context <- view handlerContext
          let mtyCon = typeRepTyCon <$> mrep
          liftIO $ putStrLn $ show (reverse context) ++ " " ++ show mtyCon ++ " - " ++ message
          pure ()
  Logger
    { log = logFor Nothing,
      logFor
    }
