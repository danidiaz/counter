{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | A repository implementation that uses an in-memory map.
module Dep.Repository.Memory (alloc, make) where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Cont
import Data.IORef
import Data.Map.Strict as Map (Map)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, diffUTCTime)
import Data.Tuple (swap)
import Dep.Has
import Dep.Has.Call
import Dep.Logger (LogLevel (..), Logger (log))
import Dep.Repository
  ( Missing (Missing),
    Repository (..),
    RunWithExistingResource (RunWithExistingResource),
    RunWithResource (..),
  )
import Prelude hiding (log)
import Control.Monad
import Dep.Clock
import Data.Time.Clock
import Control.Concurrent (threadDelay)

alloc :: MonadIO m => m (IORef (Map k v))
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
  IORef (Map rid resource) ->
  deps ->
  (ContT () m (), Repository rid resource m)
make getLastUpdated ref (Call φ) =
  ( let activity = forever do
          liftIO $ threadDelay 10e6
          φ log Debug "Cleaning stale entries..."
          now <- φ getNow
          let notStale (getLastUpdated -> lastUpdated) = 
                diffUTCTime now lastUpdated < secondsToNominalDiffTime 30
          liftIO do atomicModifyIORef' ref (\m -> (Map.filter notStale m, ()))
     in ContT \f -> do
          runInIO <- askRunInIO
          liftIO $ withAsync (runInIO activity) \_ -> runInIO (f ()),
    do
      let withResource k = do
            φ log Debug "withResource"
            pure $
              RunWithResource \callback -> do
                liftIO do atomicModifyIORef' ref (swap . Map.alterF callback k)
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
       in Repository
            { withResource,
              withExistingResource
            }
  )