{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

-- | A repository implementation that uses an in-memory map.
module Dep.Repository.Memory (alloc, make) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Cont
import Data.Aeson
import Data.IORef
import Data.Map.Strict as Map (Map)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, diffUTCTime)
import Data.Tuple (swap)
import Dep.Clock
import Dep.Has
import Dep.Has.Call
import Dep.Logger (LogLevel (..), Logger (log))
import Dep.Repository
  ( Missing (Missing),
    Repository (..),
    RunWithExistingResource (RunWithExistingResource),
    RunWithResource (..),
  )
import GHC.Generics (Generic)
import Prelude hiding (log)
import Data.Time.Clock (secondsToNominalDiffTime)

data Conf = Conf
  { checkIntervalSeconds :: Int,
    staleAfterSeconds :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type MapRef rid resource = IORef (Map rid resource)

alloc :: MonadIO n => n (MapRef rid resource)
alloc = liftIO $ newIORef Map.empty

make ::
  ( MonadUnliftIO m,
    Has Logger m deps,
    Has Clock m deps,
    -- The component requires itself, to enable instrumentation of
    -- self-invocations
    Has (Repository rid resource) m deps,
    Ord rid
  ) =>
  (resource -> UTCTime) ->
  m Conf ->
  MapRef rid resource ->
  deps ->
  (ContT () m (), Repository rid resource m)
make getLastUpdated askConf mapRef (Call φ) =
  ( do
      let activity = forever do
            Conf {checkIntervalSeconds} <- askConf
            liftIO $ threadDelay (checkIntervalSeconds * 1e6)
            φ log Debug "Cleaning stale entries..."
            now <- φ getNow
            Conf {staleAfterSeconds} <- askConf
            let notStale (getLastUpdated -> lastUpdated) =
                  diffUTCTime now lastUpdated < secondsToNominalDiffTime (fromIntegral staleAfterSeconds)
            liftIO do atomicModifyIORef' mapRef (\m -> (Map.filter notStale m, ()))
      ContT \f -> do
        runInIO <- askRunInIO
        liftIO $ withAsync (runInIO activity) \_ -> runInIO (f ()),
    do
      let withResource k = do
            φ log Debug "withResource"
            pure $
              RunWithResource \callback -> do
                liftIO do atomicModifyIORef' mapRef (swap . Map.alterF callback k)
          withExistingResource k = do
            φ log Debug "withExistingResource"
            -- Here we use a version of withResource taken from the
            -- dependency injection environment, which unlike the one defined
            -- above, it might have been instrumented in the composition root
            -- (to add new logging statements for example).
            -- https://stackoverflow.com/questions/56614354/why-does-self-invocation-not-work-for-spring-proxies-e-g-with-aop
            RunWithResource {runWithResource} <- φ Dep.Repository.withResource k
            pure $ RunWithExistingResource \callback ->
              runWithResource
                \mx -> case mx of
                  Nothing ->
                    (Left Missing, Nothing)
                  Just x ->
                    let (b, mr) = callback x
                     in (Right b, mr)
      Repository
        { withResource,
          withExistingResource
        }
  )