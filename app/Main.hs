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
import Servant
  ( Application,
    Get,
    Handler,
    JSON,
    NamedRoutes,
    Post,
    Proxy (Proxy),
    serve,
    type (:>), Capture,
  )
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Servant.Server.Generic (AsServerT)

type CounterId = Int

data  Counters mode = Counters
  {
    counters :: mode :- "counter" :> Capture "counterId" Int :> NamedRoutes Counter
  }
  deriving stock Generic

data Counter mode = Counter
  { increase :: mode :- "increase" :> Post '[JSON] (),
    query :: mode :- Get '[JSON] Int
  }
  deriving stock (Generic)

type CountersAPI = NamedRoutes Counters

counterServer :: IORef Int -> Counters (AsServerT Handler)
counterServer ref = Counters {
  counters = \counterId ->
    Counter
      { increase = liftIO do
          modifyIORef' ref succ,
        query = liftIO do
          readIORef ref
      }
}

main :: IO ()
main = do
  ref <- newIORef 0
  let app :: Application
      app = serve (Proxy @CountersAPI) (counterServer ref)
  run 8000 app
