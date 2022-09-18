{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.IORef
import Data.Map.Strict as Map (Map, alterF, empty)
import Data.Tuple (swap)
import Data.UUID
import Data.UUID.V4
import Network.Wai.Handler.Warp (run)
import Servant
  ( Application,
    Capture,
    Delete,
    Get,
    Handler,
    JSON,
    NamedRoutes,
    Post,
    Proxy (Proxy),
    ServerError,
    err404,
    err500,
    serve,
    type (:>),
  )
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Servant.Server.Generic (AsServerT)

type API = "counter" :> NamedRoutes CountersAPI

data CountersAPI mode = CountersAPI
  { counters :: mode :- Capture "counterId" CounterId :> NamedRoutes CounterAPI,
    create :: mode :- Post '[JSON] CounterId
  }
  deriving stock (Generic)

data CounterAPI mode = CounterAPI
  { increase :: mode :- "increase" :> Post '[JSON] (),
    query :: mode :- Get '[JSON] Int,
    delete :: mode :- Delete '[JSON] ()
  }
  deriving stock (Generic)

type CounterId = UUID

type Counter = Int

makeCounterAPI :: WithExistingResource Counter -> CounterAPI (AsServerT Handler)
makeCounterAPI withExistingResource =
  CounterAPI
    { increase = withExistingResource (\c -> (pure (), Just (succ c))),
      query = withExistingResource (\c -> (pure c, Just c)),
      delete = withExistingResource (\_ -> (pure (), Nothing))
    }

makeCountersAPI :: IORef (Map CounterId Int) -> CountersAPI (AsServerT Handler)
makeCountersAPI ref =
  CountersAPI
    { counters = \counterId -> do
        makeCounterAPI (handleMissing (withResourceInMap ref counterId)),
      create = do
        uuid <- liftIO nextRandom
        withResourceInMap ref uuid \case
          Nothing -> (Right uuid, Just 0)
          Just _ -> (Left err500, Nothing) -- UUID collision!
    }

--
-- SOME API-GENERIC HELPERS
-- Not really related to named routes.

type WithResource r = forall b. (Maybe r -> (Either ServerError b, Maybe r)) -> Handler b

type WithExistingResource r = forall b. (r -> (Either ServerError b, Maybe r)) -> Handler b

-- | Handler checking that the resource exists, and throwing 404 if it doesn't.
handleMissing :: WithResource r -> WithExistingResource r
handleMissing mightNotExist callback =
  mightNotExist
    \mx -> case mx of
      Nothing -> (Left err404, Nothing)
      Just x -> callback x

withResourceInMap :: Ord k => IORef (Map k r) -> k -> WithResource r
withResourceInMap ref k callback =
  do
    r <- liftIO do atomicModifyIORef' ref (swap . Map.alterF callback k)
    liftEither r

--
--

main :: IO ()
main = do
  ref <- newIORef Map.empty
  let app :: Application
      app = serve (Proxy @API) (makeCountersAPI ref)
  run 8000 app
