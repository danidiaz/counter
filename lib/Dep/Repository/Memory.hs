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
{-# LANGUAGE ViewPatterns #-}

module Dep.Repository.Memory where

import Control.Monad.IO.Class
import Data.IORef
import Data.Map.Strict as Map (Map)
import Data.Map.Strict qualified as Map
import Data.Result
import Data.Tuple (swap)
import Dep.Env
import Dep.Has
import Dep.Logger
import Dep.Repository
import Prelude hiding (log)

make ::
  (MonadIO m, Has Logger m env, Ord rid) =>
  IORef (Map rid resource) ->
  env ->
  Repository rid resource m
make ref (asCall -> call) = do
  let withResource k = do
        call log "withResource"
        pure $
          RunWithResource \callback -> do
            liftIO do atomicModifyIORef' ref (swap . Map.alterF callback k)
      withExistingResource k = do
        call log "withExistingResource"
        z <- withResource k
        pure $ handleMissing z
   in Repository
        { withResource,
          withExistingResource
        }
  where
    handleMissing :: MonadIO m => RunWithResource r m -> RunWithExistingResource r m
    handleMissing RunWithResource {runWithResource} = RunWithExistingResource \callback ->
      runWithResource
        \mx -> case mx of
          Nothing ->
            (Error Missing, Nothing)
          Just x ->
            let (b, mr) = callback x
             in (Ok b, mr)