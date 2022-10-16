{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Counter.Model
  ( CounterId(..),
    Counter(..),
    CounterRepository,
    CreateCounter(..),
    makeCreateCounter,
    GetCounter(..),
    makeGetCounter,
    IncreaseCounter(..),
    makeIncreaseCounter,
    DeleteCounter(..),
    makeDeleteCounter,
    Collision (..),
    Missing (..),
  )
where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor
import Data.Result
import Data.UUID
import Data.UUID.V4
import Dep.Env
import Dep.Has
import Dep.Logger
import Dep.Repository
import GHC.Generics (Generic)
import Prelude hiding (log)

newtype CounterId = CounterId UUID
  deriving stock (Show, Ord, Eq)
  deriving newtype (FromJSON, ToJSON)

data Counter = Counter
  { counterId :: CounterId,
    counterValue :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type CounterRepository = Repository CounterId Counter

newtype GetCounter m = GetCounter {getCounter :: CounterId -> m (Result Missing Counter)}

makeGetCounter :: (Monad m, Has CounterRepository m env) => env -> GetCounter m
makeGetCounter (asCall -> call) = GetCounter \counterId -> do
  RunWithExistingResource {runWithExistingResource} <- call withExistingResource counterId
  runWithExistingResource (\c -> (c, Just c))

newtype IncreaseCounter m = IncreaseCounter {increaseCounter :: CounterId -> m (Result Missing ())}

makeIncreaseCounter :: (Monad m, Has CounterRepository m env) => env -> IncreaseCounter m
makeIncreaseCounter (asCall -> call) = IncreaseCounter \counterId -> do
  RunWithExistingResource {runWithExistingResource} <- call withExistingResource counterId
  runWithExistingResource (\c@Counter {counterValue} -> ((), Just (c {counterValue = succ counterValue})))

newtype DeleteCounter m = DeleteCounter {deleteCounter :: CounterId -> m (Result Missing ())}

makeDeleteCounter :: (Monad m, Has Logger m env, Has CounterRepository m env) => env -> DeleteCounter m
makeDeleteCounter (asCall -> call) = DeleteCounter \counterId -> do
  call log $ "Requesting counter deletion " ++ show counterId
  RunWithExistingResource {runWithExistingResource} <- call withExistingResource counterId
  runWithExistingResource (\(_ :: Counter) -> ((), Nothing))

newtype CreateCounter m = CreateCounter {createCounter :: m (Result Collision CounterId)}

makeCreateCounter :: (MonadIO m, Has CounterRepository m env) => env -> CreateCounter m
makeCreateCounter (asCall -> call) = CreateCounter do
  counterId <- CounterId <$> liftIO nextRandom
  RunWithResource {runWithResource} <- call withResource counterId
  runWithResource \case
    Nothing -> (Ok counterId, Just (Counter {counterId, counterValue = 0}))
    Just _ -> (Error Collision, Nothing)

data Collision = Collision
