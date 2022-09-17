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
    type (:>),
  )
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Servant.Server.Generic (AsServerT)

data Counter mode = Counter
  { increase :: mode :- "counter" :> "increase" :> Post '[JSON] (),
    query :: mode :- "counter" :> Get '[JSON] Int
  }
  deriving stock (Generic)

type API = NamedRoutes Counter

counter :: IORef Int -> Counter (AsServerT Handler)
counter ref =
  Counter
    { increase = liftIO do
        modifyIORef' ref succ,
      query = liftIO do
        readIORef ref
    }

main :: IO ()
main = do
  ref <- newIORef 0
  let app :: Application
      app = serve (Proxy @API) (counter ref)
  run 8000 app
