{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | This module connects the Servant API with the application's model.
--
-- In particular, it defines some instances that help map datatypes to and fro. 
module Counter.Server (
  ServantServer(..),
  makeServantServer
) where

import Counter.API
import Counter.API qualified as API
import Counter.Model
import Counter.Model qualified as Model
import Data.Coerce
import Dep.Has
import Dep.Has.Call
import Data.Kind
import Servant.Server
    ( Handler,
      HasServer(ServerT),
      err404,
      err500 )
import Servant.Server.ToHandler
import Control.Monad.Reader
import Servant (ServerError)

-- | This is a marker type to identify the servant API.
-- 
-- In theory, a model could be used to serve different API's, so to avoid
-- instance collisions we parameterize many helper typeclasses by 'X'.
data X

-- | Maps a domain-relevant error to 'ServerError'.
instance Convertible X Model.Missing ServerError where
  convert _ = err404

-- | Maps a domain-relevant error to 'ServerError'.
instance Convertible X Model.Collision ServerError where
  convert _ = err500

-- | DTO mapping.
instance Convertible X API.CounterId Model.CounterId where
  convert = coerce

-- | DTO mapping.
instance Convertible X Model.CounterId API.CounterId where
  convert = coerce

-- | DTO mapping.
instance Convertible X Model.Counter API.Counter where
  convert Model.Counter {Model.counterId, Model.counterValue} =
    API.Counter
      { API.counterId = convert @X counterId,
        API.counterValue = counterValue
      }

-- | DTO mapping.
instance Convertible X () () where
  convert = id

-- | The type parameters here are a bit weird compared to other components.
--
-- @m@ is not really used as the server monad.
--
-- And we don't use @env@ for anything. It's only there becasue 'ToHandler'
-- instances require a 'ReaderT' monad to work.
type ServantServer :: Type -> (Type -> Type) -> Type
newtype ServantServer env m = ServantServer {server :: ServerT API (ReaderT env Handler)}

-- | We construct a Servant server by extracting components from the dependency
-- injection context and using them as handlers.
--
-- We need to massage the components a little because they know nothing of
-- Servant: we need to change the monad, convert model errros to
-- 'ServantError's, convert API DTOs to and from model datatypes...
makeServantServer ::
  ( m ~ ReaderT env IO,
    Has GetCounter m deps,
    Has IncreaseCounter m deps,
    Has DeleteCounter m deps,
    Has CreateCounter m deps
  ) =>
  deps ->
  ServantServer env m
makeServantServer (Call φ) = ServantServer
  \(_ :: User) ->
    CounterCollectionAPI
      { counters = \counterId -> do
          CounterAPI
            { increase = toHandler @X (φ Model.increaseCounter) counterId,
              query = toHandler @X (φ Model.getCounter) counterId,
              delete = toHandler @X (φ Model.deleteCounter) counterId
            },
        create = toHandler @X (φ Model.createCounter)
      }
