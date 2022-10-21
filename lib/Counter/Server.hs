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

-- | This is a marker type to identify the servant API.
-- 
-- In theory, a model could be used to serve different API's, so to avoid
-- instance collisions we parameterize many helper typeclasses by 'X'.
data X

-- | Maps a domain-relevant error to 'ServerError'.
instance ToServerError X Model.Missing where
  toServerError _ = err404

-- | Maps a domain-relevant error to 'ServerError'.
instance ToServerError X Model.Collision where
  toServerError _ = err500

-- | DTO mapping.
instance FromDTO X API.CounterId Model.CounterId where
  fromDTO = coerce

-- | DTO mapping.
instance ToDTO X API.CounterId Model.CounterId where
  toDTO = coerce

-- | DTO mapping.
instance ToDTO X API.Counter Model.Counter where
  toDTO Model.Counter {Model.counterId, Model.counterValue} =
    API.Counter
      { API.counterId = toDTO @X counterId,
        API.counterValue = counterValue
      }

-- | DTO mapping.
instance ToDTO X () () where
  toDTO = id

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
    Has GetCounter m cauldron,
    Has IncreaseCounter m cauldron,
    Has DeleteCounter m cauldron,
    Has CreateCounter m cauldron
  ) =>
  cauldron ->
  ServantServer env m
makeServantServer (Call call) = ServantServer
  \(_ :: User) ->
    CounterCollectionAPI
      { counters = \counterId -> do
          CounterAPI
            { increase = toHandler @X (call Model.increaseCounter) counterId,
              query = toHandler @X (call Model.getCounter) counterId,
              delete = toHandler @X (call Model.deleteCounter) counterId
            },
        create = toHandler @X (call Model.createCounter)
      }
