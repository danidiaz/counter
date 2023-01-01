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
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

-- | This is the application's "model".
--
-- Notice that it's completely ignorant of the Servant API.
--
-- Each operation is wrapped in a newtype parameterized by a monad, and it
-- obtains its dependencies (like loggers) using a helper 'Has' typeclass. This
-- is done to fit the dependency injection framework we're using.
module Counter.Model
  ( CounterId (..),
    Counter (..),
    CounterRepository,
    CreateCounter (..),
    makeCreateCounter,
    GetCounter (..),
    makeGetCounter,
    IncreaseCounter (..),
    makeIncreaseCounter,
    DeleteCounter (..),
    makeDeleteCounter,
    Collision (..),
    Missing (..),
  )
where

import Control.Monad.IO.Class
import Data.Functor
import Data.Time (UTCTime)
import Data.UUID
import Data.UUID.V4
import Dep.Clock
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
    counterValue :: Int,
    lastUpdated :: UTCTime
  }
  deriving stock (Show, Generic)

-- we could have ToJSON here, but for logging and debugging, not for REST serialization

type CounterRepository = Repository CounterId Counter

newtype GetCounter m = GetCounter {getCounter :: CounterId -> m (Either Missing Counter)}

makeGetCounter :: (Monad m, Has CounterRepository m deps) => deps -> GetCounter m
makeGetCounter (Call φ) = GetCounter \counterId -> do
  RunWithExistingResource {runWithExistingResource} <- φ withExistingResource counterId
  runWithExistingResource (\c -> (c, Just c))

newtype IncreaseCounter m = IncreaseCounter {increaseCounter :: CounterId -> m (Either Missing ())}

makeIncreaseCounter ::
  ( Monad m,
    Has Clock m deps,
    Has CounterRepository m deps
  ) =>
  deps ->
  IncreaseCounter m
makeIncreaseCounter (Call φ) = IncreaseCounter \counterId -> do
  now <- φ getNow
  RunWithExistingResource {runWithExistingResource} <- φ withExistingResource counterId
  runWithExistingResource (\c@Counter {counterValue} -> ((), Just (c {counterValue = succ counterValue, lastUpdated = now})))

newtype DeleteCounter m = DeleteCounter {deleteCounter :: CounterId -> m (Either Missing ())}

makeDeleteCounter ::
  ( Monad m,
    Has Logger m deps,
    Has CounterRepository m deps
  ) =>
  deps ->
  DeleteCounter m
makeDeleteCounter (Call φ) = DeleteCounter \counterId -> do
  φ log Info $ "Requesting counter deletion " ++ show counterId
  RunWithExistingResource {runWithExistingResource} <- φ withExistingResource counterId
  runWithExistingResource (\(_ :: Counter) -> ((), Nothing))

newtype CreateCounter m = CreateCounter {createCounter :: m (Either Collision CounterId)}

makeCreateCounter ::
  ( MonadIO m,
    Has Clock m deps,
    Has CounterRepository m deps
  ) =>
  deps ->
  CreateCounter m
makeCreateCounter (Call φ) = CreateCounter do
  counterId <- CounterId <$> liftIO nextRandom
  now <- φ getNow
  RunWithResource {runWithResource} <- φ withResource counterId
  runWithResource \case
    Nothing -> (Right counterId, Just (Counter {counterId, counterValue = 0, lastUpdated = now}))
    Just _ -> (Left Collision, Nothing)

-- | This is a domain-relevant error.
data Collision = Collision
