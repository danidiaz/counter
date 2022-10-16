{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Counter.Main where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Counter.API
import Counter.Model
import Counter.Model qualified as Model
import Counter.Server
import Data.Functor
import Data.IORef
import Data.Map.Strict as Map (empty)
import Data.Proxy
import Dep.Env
import Dep.Has
import Dep.Logger
import Dep.Logger.HandlerAware
import Dep.Repository.Memory
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant.Server
    ( serveWithContext,
      HasServer(hoistServerWithContext, ServerT),
      BasicAuthCheck,
      Application )
import Servant.Server.HandlerContext
import Servant.Server.ToHandler

data CompositionRoot h m = CompositionRoot
  { _logger :: h (Logger m),
    _getCounter :: h (GetCounter m),
    _increaseCounter :: h (IncreaseCounter m),
    _deleteCounter :: h (DeleteCounter m),
    _createCounter :: h (CreateCounter m),
    _counterRepository :: h (Model.CounterRepository m)
  }
  deriving stock (Generic)
  deriving anyclass (FieldsFindableByType, Phased)

type Phases m = ContT () IO `Compose` Constructor (CompositionRoot Identity m)

root :: (MonadIO m, MonadReader env m, HasHandlerContext env) => CompositionRoot (Phases m) m
root =
  CompositionRoot
    { _logger = fromBare $ noAlloc $ noDeps Dep.Logger.HandlerAware.make,
      _getCounter = fromBare $ noAlloc makeGetCounter,
      _increaseCounter = fromBare $ noAlloc makeIncreaseCounter,
      _deleteCounter = fromBare $ noAlloc makeDeleteCounter,
      _createCounter = fromBare $ noAlloc makeCreateCounter,
      _counterRepository =
        fromBare $
          alloc (newIORef Map.empty)
            <&> \ref -> Dep.Repository.Memory.make ref
    }
  where
    alloc :: IO a -> ContT () IO a
    alloc = liftIO
    noAlloc :: a -> ContT () IO a
    noAlloc = pure
    noDeps :: x -> env -> x
    noDeps = const

deriving via Autowired (CompositionRoot Identity m) instance Autowireable r_ m (CompositionRoot Identity m) => Has r_ m (CompositionRoot Identity m)

makeServer :: CompositionRoot Identity M' -> ServerT API M
makeServer (asCall -> call) = \(_ :: User) ->
  CountersAPI
    { counters = \(fromDTO @X -> counterId) -> do
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
        server = addHandlerContext [] $ makeServer ready
    env <- makeServerEnv
    -- https://docs.servant.dev/en/stable/cookbook/hoist-server-with-context/HoistServerWithContext.html
    let server' = hoistServerWithContext (Proxy @API) (Proxy @'[BasicAuthCheck User]) (`runReaderT` env) server
        app :: Application
        app = serveWithContext (Proxy @API) basicAuthServerContext server'
    run 8000 app