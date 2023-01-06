{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module connects the Servant API with the application's model.
--
-- In particular, it defines some instances that help map datatypes to and fro.
module Counter.Server
  ( CounterServer (..),
    makeCounterServer,
  )
where

import Counter.API
import Counter.API qualified as API
import Counter.Model
import Counter.Model qualified as Model
import Data.Kind
import Dep.Has
import Dep.Handler
import Servant (ServerError)
import Servant.Server
  ( 
    HasServer (ServerT),
    err404,
    err500,
  )
import Dep.Clock
import Dep.Has.Call

-- | This is a marker type to identify the servant API.
type X :: (Type -> Type) -> Type
data X m = X { 
    counter2counter :: Model.Counter -> m API.Counter
  }

-- | Maps a domain-relevant error to 'ServerError'.
instance Convertible X Model.Missing ServerError where
  convert = convertConst err404

-- | Maps a domain-relevant error to 'ServerError'.
instance Convertible X Model.Collision ServerError where
  convert = convertConst err500

-- | DTO mapping.
instance Convertible X API.CounterId Model.CounterId where
  convert = convertCoerce

instance Convertible X Model.CounterId API.CounterId where
  convert = convertCoerce

-- | DTO mapping.
-- 
-- This is an example of conversion that performs effects and has dependencies
-- on other components.
instance Convertible X Model.Counter API.Counter where
  convert X {counter2counter} = counter2counter

makeX :: (Monad m , Has Clock m deps) => deps -> X m
makeX (Call φ) = 
  let converter = X {
        counter2counter = \Model.Counter {Model.counterId, Model.counterValue} -> do
          counterId' <- convert converter counterId
          convertedAt <- φ getNow
          pure
            API.Counter
              { API.counterId = counterId',
                API.counterValue = counterValue,
                convertedAt
              }
      }
    in converter


-- | DTO mapping.
instance Convertible X () () where
  convert = convertId

-- | The type parameters here are a bit weird compared to other components.
--
-- @m@ is not really used as the server monad.
--
-- And we don't use @env@ for anything. It's only there becasue 'ToHandler'
-- instances require a 'ReaderT' monad to work.
type CounterServer :: Type -> (Type -> Type) -> Type
newtype CounterServer env m = CounterServer {counterServer :: ServerT API (RHandler env)}

-- | We construct a Servant server by extracting components from the dependency
-- injection context and using them as handlers.
--
-- We need to massage the components a little because they know nothing of
-- Servant: we need to change the monad, convert model errros to
-- 'ServantError's, convert API DTOs to and from model datatypes...
--
-- See also "Dep.Server".
makeCounterServer ::
  ( 
    m ~ RIO env,
    Has GetCounter m deps,
    Has IncreaseCounter m deps,
    Has DeleteCounter m deps,
    Has CreateCounter m deps,
    Has Clock m deps
  ) =>
  -- |
  deps ->
  -- |
  CounterServer env m
makeCounterServer deps = CounterServer
  let HandlerCall η = asHandlerCall deps (makeX deps)
   in
      \(_ :: User) ->
        CounterCollectionAPI
          { counters = \counterId -> do
              CounterAPI
                { increase = η Model.increaseCounter counterId,
                  query = η Model.getCounter counterId,
                  delete = η Model.deleteCounter counterId
                },
            create = η Model.createCounter
          }
