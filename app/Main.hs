{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import Data.IORef
import Network.Wai.Handler.Warp (run)
import Data.Map.Strict as Map
import Data.Set as Set

import Servant
  ( Application,
    Get,
    Handler,
    JSON,
    NamedRoutes,
    Post,
    Proxy (Proxy),
    serve,
    type (:>), Capture, err404,
  )
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Servant.Server.Generic (AsServerT)
import Control.Monad.Error.Class

type CounterId = Int

data CountersAPI mode = CountersAPI
  {
    withCounter :: mode :- Capture "counterId" Int :> NamedRoutes CounterAPI,
    create :: mode :- Post '[JSON] CounterId
  }
  deriving stock Generic

data CounterAPI mode = CounterAPI
  { increase :: mode :- "increase" :> Post '[JSON] (),
    query :: mode :- Get '[JSON] Int
  }
  deriving stock (Generic)

type API = "counter" :> NamedRoutes CountersAPI

makeCountersAPI :: IORef (Map CounterId Int) -> CountersAPI (AsServerT Handler)
makeCountersAPI ref = CountersAPI {
  withCounter = \counterId ->
    CounterAPI
      { increase = do
          r <- liftIO 
            do atomicModifyIORef' ref \counterMap ->
                  case Map.lookup counterId counterMap of
                    Nothing -> do
                      (counterMap, Left err404)
                    Just counterValue ->  do
                      (Map.insert counterId (succ counterValue) counterMap, Right ())
          liftEither r,
        query = do
          r <- liftIO 
            do atomicModifyIORef' ref \counterMap ->
                  case Map.lookup counterId counterMap of
                    Nothing -> do
                      (counterMap, Left err404)
                    Just counterValue ->  do
                      (counterMap, Right counterValue)
          liftEither r
      },
  create = do
      liftIO 
        do atomicModifyIORef' ref \counterMap ->
              case Set.lookupMax (Map.keysSet counterMap) of
                Nothing -> do
                  let nextCounterId = 0
                  (Map.insert nextCounterId 0 counterMap, nextCounterId)
                Just maxCounterId ->  do
                  let nextCounterId = succ maxCounterId
                  (Map.insert nextCounterId 0 counterMap, nextCounterId)
}

main :: IO ()
main = do
  ref <- newIORef Map.empty
  let app :: Application
      app = serve (Proxy @API) (makeCountersAPI ref)
  run 8000 app
