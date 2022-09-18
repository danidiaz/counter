{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

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
type Counter = Int

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

type UseResource r = forall b . (r -> (Maybe r, b)) -> Handler b

makeCounterAPI :: UseResource Counter -> CounterAPI (AsServerT Handler)
makeCounterAPI useCounter =
    CounterAPI
      { increase = useCounter (\c -> (Just (succ c),())),
        query = useCounter (\c -> (Just c,c))
      }


makeCountersAPI :: IORef (Map CounterId Int) -> CountersAPI (AsServerT Handler)
makeCountersAPI ref = CountersAPI {
  withCounter = \counterId -> do
    let useCounter :: UseResource Counter
        useCounter callback = 
            do r <- liftIO 
                  do atomicModifyIORef' ref \counterMap ->
                      case Map.lookup counterId counterMap of
                        Nothing -> do
                          (counterMap, Left err404)
                        Just counterValue -> do
                          let (c',b) = callback counterValue 
                          case c' of
                            Nothing -> 
                              (Map.delete counterId counterMap, Right b)
                            Just newCounter ->
                              (Map.insert counterId newCounter counterMap, Right b)
               liftEither r
    makeCounterAPI useCounter,
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
