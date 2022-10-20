{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- | This is the application's "model".
--
-- Notice that it's completely ignorant of the Servant API.
--
-- Each operation is wrapped in a newtype parameterized by a monad, and it
-- obtains its dependencies (like loggers) using a helper 'Has' typeclass. This
-- is done to fit the dependency injection framework we're using.
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
import Data.Functor
import Data.Result
import Data.UUID
import Data.UUID.V4
import Dep.Has
import Dep.Has.Call
import Dep.Logger
import Dep.Repository
import GHC.Generics (Generic)
import Prelude hiding (log)

newtype CounterId = CounterId UUID
  deriving stock (Show, Ord, Eq)
  -- we could have ToJSON here, but for logging and debugging, not for REST serialization

data Counter = Counter
  { counterId :: CounterId,
    counterValue :: Int
  }
  deriving stock (Show, Generic)
  -- we could have ToJSON here, but for logging and debugging, not for REST serialization

type CounterRepository = Repository CounterId Counter

newtype GetCounter m = GetCounter {getCounter :: CounterId -> m (Result Missing Counter)}

makeGetCounter :: (Monad m, Has CounterRepository m env) => env -> GetCounter m
makeGetCounter (Call call) = GetCounter \counterId -> do
  RunWithExistingResource {runWithExistingResource} <- call withExistingResource counterId
  runWithExistingResource (\c -> (c, Just c))

newtype IncreaseCounter m = IncreaseCounter {increaseCounter :: CounterId -> m (Result Missing ())}

makeIncreaseCounter :: (Monad m, Has CounterRepository m env) => env -> IncreaseCounter m
makeIncreaseCounter (Call call) = IncreaseCounter \counterId -> do
  RunWithExistingResource {runWithExistingResource} <- call withExistingResource counterId
  runWithExistingResource (\c@Counter {counterValue} -> ((), Just (c {counterValue = succ counterValue})))

newtype DeleteCounter m = DeleteCounter {deleteCounter :: CounterId -> m (Result Missing ())}

makeDeleteCounter :: (Monad m, Has Logger m env, Has CounterRepository m env) => env -> DeleteCounter m
makeDeleteCounter (Call call) = DeleteCounter \counterId -> do
  call log $ "Requesting counter deletion " ++ show counterId
  RunWithExistingResource {runWithExistingResource} <- call withExistingResource counterId
  runWithExistingResource (\(_ :: Counter) -> ((), Nothing))

newtype CreateCounter m = CreateCounter {createCounter :: m (Result Collision CounterId)}

makeCreateCounter :: (MonadIO m, Has CounterRepository m env) => env -> CreateCounter m
makeCreateCounter (Call call) = CreateCounter do
  counterId <- CounterId <$> liftIO nextRandom
  RunWithResource {runWithResource} <- call withResource counterId
  runWithResource \case
    Nothing -> (Ok counterId, Just (Counter {counterId, counterValue = 0}))
    Just _ -> (Error Collision, Nothing)

-- | This is a domain-relevant error.
data Collision = Collision
