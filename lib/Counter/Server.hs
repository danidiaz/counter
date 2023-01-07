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
    C,
    makeC,
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
    -- These constraints can easily be added from suggestions from the IDE
    Convertible c API.CounterId Model.CounterId, 
    Convertible c Missing ServerError, 
    Convertible c Collision ServerError, 
    Convertible c () (), 
    Convertible c Model.Counter API.Counter,
    Convertible c Model.CounterId API.CounterId
  ) =>
  -- | We are polymorphic on the model <-> API converter. 
  -- It's passed positionally; it's not found by type, using 'Has'. 
  -- We *don't* need to know its concrete type, because we only use it through its instances.
  -- Despite that, the concrete converter is defined in this same module, for simplicity.
  -- But we could change it, if needed.
  c m ->
  -- |
  deps ->
  -- |
  CounterServer env m
makeCounterServer c (asHandlerCall c -> HandlerCall η) = CounterServer
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


-- | 
-- A model <-> API converter. 
-- It's almost a trivial ()-like type, because most of the conversions are pure
-- and are defined in the respective 'Convertible' instances, but for more
-- complicated conversions that require effects, we need to have
-- those functions ready.
type C :: (Type -> Type) -> Type
data C m = C { 
    counter2counter :: Model.Counter -> m API.Counter
  }

-- | The constructor for the converter.
-- It has dependencies because one of the conversions needs access to the
-- current time, and we need to create and store the conversion function (which
-- will be later used by the 'Convertible' instance)
makeC :: (Monad m , Has Clock m deps) => deps -> C m
makeC (Call φ) = 
  let converter = C {
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
-- 
-- This is an example of conversion that performs effects and has dependencies
-- on other components.
instance Convertible C Model.Counter API.Counter where
  convert C {counter2counter} = counter2counter


-- | Maps a domain-relevant error to 'ServerError'.
instance Convertible C Model.Missing ServerError where
  convert = convertConst err404

-- | Maps a domain-relevant error to 'ServerError'.
instance Convertible C Model.Collision ServerError where
  convert = convertConst err500

-- | DTO mapping.
instance Convertible C API.CounterId Model.CounterId where
  convert = convertCoerce

instance Convertible C Model.CounterId API.CounterId where
  convert = convertCoerce

-- | DTO mapping.
instance Convertible C () () where
  convert = convertId

-- | The type parameters here are a bit weird compared to other components.
--
-- @m@ is not really used as the server monad.
--
-- And we don't use @env@ for anything. It's only there becasue 'ToHandler'
-- instances require a 'ReaderT' monad to work.
type CounterServer :: Type -> (Type -> Type) -> Type
newtype CounterServer env m = CounterServer {counterServer :: ServerT API (RHandler env)}

