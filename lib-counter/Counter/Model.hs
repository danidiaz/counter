{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Counter.Model where

import Data.UUID
import Control.Monad.IO.Class
import Data.Map.Strict as Map (Map, empty)
import Data.Map.Strict qualified as Map
import Data.Tuple (swap)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.IORef
import Dep.Env
import Dep.Has
import Data.UUID
import Data.UUID.V4
import Data.Functor

newtype CounterId = CounterId UUID 
  deriving stock (Ord, Eq)

data Counter = Counter {
    counterId :: CounterId,
    counterValue :: Int
}

newtype GetCounter m = 
  GetCounter { getCounter :: CounterId -> m (Result Missing Counter) }

makeGetCounter :: (Monad m, Has CounterRepository m env) => env -> GetCounter m
makeGetCounter (asCall -> call) = GetCounter \counterId -> do
    WithExistingResource {runWithExistingResource} <- call withExistingCounter counterId 
    runWithExistingResource (\c -> (c, Just c))

newtype IncreaseCounter m = 
  IncreaseCounter { increaseCounter :: CounterId -> m (Result Missing ()) }

makeIncreaseCounter ::  (Monad m, Has CounterRepository m env) => env -> IncreaseCounter m
makeIncreaseCounter (asCall -> call) = IncreaseCounter \counterId -> do
    WithExistingResource {runWithExistingResource} <- call withExistingCounter counterId 
    runWithExistingResource (\c@Counter {counterValue} -> ((), Just (c {counterValue = succ counterValue})))

newtype DeleteCounter m = 
  DeleteCounter { deleteCounter :: CounterId -> m (Result Missing ()) }

makeDeleteCounter ::  (Monad m, Has CounterRepository m env) => env -> DeleteCounter m
makeDeleteCounter (asCall -> call) = DeleteCounter \counterId -> do
    WithExistingResource {runWithExistingResource} <- call withExistingCounter counterId 
    runWithExistingResource (\_ -> ((), Nothing))

newtype CreateCounter m = 
  CreateCounter { createCounter :: m (Result Collision CounterId) }

makeCreateCounter ::  (MonadIO m, Has CounterRepository m env) => env -> CreateCounter m
makeCreateCounter (asCall -> call) = CreateCounter do
    counterId <- CounterId <$> liftIO nextRandom
    WithResource {runWithResource} <- call withCounter counterId 
    runWithResource \case
      Nothing -> (Problem Collision, Just (Counter {counterId, counterValue = 0}))
      Just _ -> (Result counterId, Nothing) -- UUID collision!

-- TODO: generalize this.
data CounterRepository m = CounterRepository {
    withCounter :: CounterId -> m (WithResource Counter m),
    withExistingCounter :: CounterId -> m (WithExistingResource Counter m) 
}

  -- ref <- 
makeInMemoryCounterRepository :: MonadIO m => IORef (Map CounterId Counter) -> CounterRepository m
makeInMemoryCounterRepository ref = do
  let withCounter k = pure $ 
        WithResource \callback -> do
          liftIO do atomicModifyIORef' ref (swap . Map.alterF callback k)
      withExistingCounter k = do
        z <- withCounter k
        pure $ handleMissing z 
   in CounterRepository {
      withCounter,
      withExistingCounter
    }

--
--
-- Like 'WithResource' but we assume the resource exists.
newtype WithResource r m = WithResource
  { runWithResource :: forall b. (Maybe r -> (b, Maybe r)) -> m b
  }

newtype WithExistingResource r m = WithExistingResource
  { runWithExistingResource ::
      forall b.
      (r -> (b, Maybe r)) ->
      m (Result Missing b)
  }

data Result e a = 
    Problem e
  | Result a

data Missing = Missing

data Collision = Collision

-- | Takes care of checking if the resource exists, throwing 404 if it doesn't.
handleMissing :: MonadIO m => WithResource r m -> WithExistingResource r m
handleMissing WithResource {runWithResource} = WithExistingResource \callback ->
  runWithResource
    \mx -> case mx of
      Nothing -> 
        (Problem Missing, Nothing)
      Just x -> 
        let (b, mr) = callback x
         in (Result b, mr) 
