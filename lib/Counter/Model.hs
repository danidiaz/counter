{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Dep.Has
import Dep.Env
import Dep.Has
import Data.UUID
import Data.UUID.V4
import Data.Functor
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Result
import Dep.Logger
import Prelude hiding (log)

newtype CounterId = CounterId UUID 
  deriving stock (Show, Ord, Eq)
  deriving newtype (FromJSON, ToJSON)

data Counter = Counter {
    counterId :: CounterId,
    counterValue :: Int
} deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)


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

makeDeleteCounter ::  (Monad m, Has Logger m env, Has CounterRepository m env) => env -> DeleteCounter m
makeDeleteCounter (asCall -> call) = DeleteCounter \counterId -> do
    call log $ "Requesting counter deletion " ++ show counterId
    WithExistingResource {runWithExistingResource} <- call withExistingCounter counterId 
    runWithExistingResource (\_ -> ((), Nothing))

newtype CreateCounter m = 
  CreateCounter { createCounter :: m (Result Collision CounterId) }

makeCreateCounter ::  (MonadIO m, Has CounterRepository m env) => env -> CreateCounter m
makeCreateCounter (asCall -> call) = CreateCounter do
    counterId <- CounterId <$> liftIO nextRandom
    WithResource {runWithResource} <- call withCounter counterId 
    runWithResource \case
      Nothing -> (Ok counterId, Just (Counter {counterId, counterValue = 0})) 
      Just _ -> (Error Collision, Nothing)

-- TODO: generalize this.
data CounterRepository m = CounterRepository {
    withCounter :: CounterId -> m (WithResource Counter m),
    withExistingCounter :: CounterId -> m (WithExistingResource Counter m) 
}

makeInMemoryCounterRepository :: (MonadIO m, Has Logger m env) => 
  IORef (Map CounterId Counter) 
  -> env
  -> CounterRepository m
makeInMemoryCounterRepository ref (asCall -> call) = do
  let withCounter k = do
        call log "withCounter"
        pure $ 
          WithResource \callback -> do
            liftIO do atomicModifyIORef' ref (swap . Map.alterF callback k)
      withExistingCounter k = do
        call log "withExistingCounter"
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

data Missing = Missing

data Collision = Collision

-- | Takes care of checking if the resource exists, throwing 404 if it doesn't.
handleMissing :: MonadIO m => WithResource r m -> WithExistingResource r m
handleMissing WithResource {runWithResource} = WithExistingResource \callback ->
  runWithResource
    \mx -> case mx of
      Nothing -> 
        (Error Missing, Nothing)
      Just x -> 
        let (b, mr) = callback x
         in (Ok b, mr) 
