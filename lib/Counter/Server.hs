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

-- | This module connects the Servant API with the application's model.
--
-- In particular, it defines some instances that help map datatypes to and fro.
module Counter.Server
  ( ServantServer (..),
    makeServantServer,
  )
where

import Control.Monad.Reader
import Counter.API
import Counter.API qualified as API
import Counter.Model
import Counter.Model qualified as Model
import Data.Coerce
import Data.Kind
import Dep.Has
import Dep.Has.Call
import Servant (ServerError)
import Servant.Server
  ( Handler,
    HasServer (ServerT),
    err404,
    err500,
  )
import Servant.Server.ToHandler

-- | This is a marker type to identify the servant API.
--
-- In theory, a model could be used to serve different API's, so to avoid
-- instance collisions we parameterize many helper typeclasses by 'X'.
data X

-- | Maps a domain-relevant error to 'ServerError'.
instance Monad m => Convertible X m deps Model.Missing ServerError where
  convert _ _ = pure err404

-- | Maps a domain-relevant error to 'ServerError'.
instance Monad m => Convertible X m deps Model.Collision ServerError where
  convert _ _ = pure err500

-- | DTO mapping.
instance Monad m => Convertible X m deps API.CounterId Model.CounterId where
  convert _ x = pure $ coerce x

-- | DTO mapping.
instance Monad m => Convertible X m deps Model.CounterId API.CounterId where
  convert _ x = pure $ coerce x

-- | DTO mapping.
instance Monad m => Convertible X m deps Model.Counter API.Counter where
  convert deps Model.Counter {Model.counterId, Model.counterValue} = do
    counterId' <- convert @X deps counterId
    pure
      API.Counter
        { API.counterId = counterId',
          API.counterValue = counterValue
        }

-- | DTO mapping.
instance Monad m => Convertible X m deps () () where
  convert _ x = pure x

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
makeServantServer deps@(Call φ) = ServantServer
  \(_ :: User) ->
    CounterCollectionAPI
      { counters = \counterId -> do
          CounterAPI
            { increase = toH (φ Model.increaseCounter) counterId,
              query = toH (φ Model.getCounter) counterId,
              delete = toH (φ Model.deleteCounter) counterId
            },
        create = toH (φ Model.createCounter)
      }
  where
    HandlerConverter toH = makeHandlerConverter @X deps