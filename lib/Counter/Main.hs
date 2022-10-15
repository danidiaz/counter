{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Counter.Main where

import Control.Monad.Trans.Reader
import Counter.API
import Counter.Server
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant.Server (Application, BasicAuthCheck, hoistServerWithContext, serveWithContext)
import Control.Monad.IO.Class
import Data.Functor

import Dep.Env
import Counter.Model
import qualified Counter.Model as Model

import GHC.Generics (Generic)
import Control.Monad.Trans.Cont
import Data.Map.Strict as Map (Map, empty)
import Data.IORef
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.Server
import Servant.Server.Handler
import Dep.Has
import qualified Counter.API as Model

data CompositionRoot h m = CompositionRoot {
  _getCounter :: h (GetCounter m), 
  _increaseCounter :: h (IncreaseCounter m), 
  _deleteCounter :: h (DeleteCounter m), 
  _createCounter :: h (CreateCounter m),
  _counterRepository :: h (CounterRepository m)
} deriving stock (Generic) 
  deriving anyclass (FieldsFindableByType, Phased)

type Phases m = ContT () IO `Compose` Constructor (CompositionRoot Identity m)

root :: MonadIO m => CompositionRoot (Phases m) m 
root = CompositionRoot {
  _getCounter = fromBare $ noAlloc makeGetCounter,
  _increaseCounter = fromBare $ noAlloc makeIncreaseCounter,
  _deleteCounter = fromBare $ noAlloc makeDeleteCounter,
  _createCounter = fromBare $ noAlloc makeCreateCounter,
  _counterRepository = fromBare $ 
    alloc (newIORef Map.empty) 
      <&> \ref _ -> makeInMemoryCounterRepository ref
}
  where 
    alloc :: IO a -> ContT () IO a
    alloc = liftIO
    noAlloc :: a -> ContT () IO a
    noAlloc = pure

deriving via Autowired (CompositionRoot Identity m) instance Autowireable r_ m (CompositionRoot Identity m) => Has r_ m (CompositionRoot Identity m)

makeServer :: CompositionRoot Identity M' -> ServerT API M
makeServer (asCall -> call) = \user -> CountersAPI
    { counters = \counterId -> do
         CounterAPI
              { increase = toHandler @X (call Model.increaseCounter counterId), 
                query = toHandler @X (call Model.getCounter counterId),
                delete = toHandler @X (call Model.deleteCounter counterId)
              },
      create = toHandler @X (call Model.createCounter)
    }


main :: IO ()
main = do
  runContT (pullPhase root) \allocated -> do
    let ready :: CompositionRoot Identity M' = fixEnv allocated
        server = makeServer ready
    env <- makeServerEnv
    -- https://docs.servant.dev/en/stable/cookbook/hoist-server-with-context/HoistServerWithContext.html
    let server' = hoistServerWithContext (Proxy @API) (Proxy @'[BasicAuthCheck User]) (`runReaderT` env) server
        app :: Application
        app = serveWithContext (Proxy @API) basicAuthServerContext server'
    run 8000 app