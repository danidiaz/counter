{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
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
module Dep.Repository.Memory (make) where

import Control.Monad.IO.Class
import Data.IORef
import Data.Map.Strict as Map (Map)
import Data.Map.Strict qualified as Map
import Data.Result
import Data.Tuple (swap)
import Dep.Has
import Dep.Has.Call
import Dep.Logger (Logger (log))
import Dep.Repository
  ( Missing (Missing),
    Repository (..),
    RunWithExistingResource (RunWithExistingResource),
    RunWithResource (..),
  )
import Prelude hiding (log)

make ::
  ( MonadIO m,
    Has Logger m env,
    -- The component requires itself, to enable instrumentation of
    -- self-invocations
    Has (Repository rid resource) m env,
    Ord rid
  ) =>
  IORef (Map rid resource) ->
  env ->
  Repository rid resource m
make ref (Call φ) = do
  let withResource k = do
        φ log "withResource"
        pure $
          RunWithResource \callback -> do
            liftIO do atomicModifyIORef' ref (swap . Map.alterF callback k)
      withExistingResource k = do
        φ log "withExistingResource"
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
                (Error Missing, Nothing)
              Just x ->
                let (b, mr) = callback x
                 in (Ok b, mr)
   in Repository
        { withResource,
          withExistingResource
        }