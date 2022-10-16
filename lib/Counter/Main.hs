{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.Kind
import Data.Map.Strict as Map (empty)
import Data.Proxy
import Dep.Env
    ( Identity(Identity),
      Compose(Compose),
      fixEnv,
      fromBare,
      pullPhase,
      Autowireable,
      Autowired(..),
      Constructor,
      FieldsFindableByType,
      Phased )
import Dep.Has ( Has, asCall )
import Dep.Logger
import Dep.Logger.HandlerAware
import Dep.Repository.Memory
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant.Server
  ( Application,
    BasicAuthCheck,
    Handler,
    HasServer (ServerT, hoistServerWithContext),
    serveWithContext,
  )
import Servant.Server.HandlerContext
import Servant.Server.ToHandler

data Cauldron h m = Cauldron
  { _logger :: h (Logger m),
    _getCounter :: h (GetCounter m),
    _increaseCounter :: h (IncreaseCounter m),
    _deleteCounter :: h (DeleteCounter m),
    _createCounter :: h (CreateCounter m),
    _counterRepository :: h (Model.CounterRepository m)
  }
  deriving stock (Generic)
  deriving anyclass (FieldsFindableByType, Phased)

type Allocator = ContT () IO

type Phases m = Allocator `Compose` Constructor (Cauldron Identity m)

cauldron :: (MonadIO m, MonadReader env m, HasHandlerContext env) => Cauldron (Phases m) m
cauldron =
  Cauldron
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

deriving via Autowired (Cauldron Identity m) instance Autowireable r_ m (Cauldron Identity m) => Has r_ m (Cauldron Identity m)

type M :: Type -> Type
type M = ReaderT Env Handler

type M' :: Type -> Type
type M' = ReaderT Env IO

makeServer :: Cauldron Identity M' -> ServerT API M
makeServer (asCall -> call) = \(_ :: User) ->
  CounterCollectionAPI
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
  runContT (pullPhase cauldron) \allocated -> do
    let wired :: Cauldron Identity M' = fixEnv allocated
        server = addHandlerContext [] $ makeServer wired
    env <- makeServerEnv
    -- https://docs.servant.dev/en/stable/cookbook/hoist-server-with-context/HoistServerWithContext.html
    let hoistedServer =
          hoistServerWithContext
            (Proxy @API)
            (Proxy @'[BasicAuthCheck User])
            (`runReaderT` env)
            server
        app :: Application
        app = serveWithContext (Proxy @API) basicAuthServerContext hoistedServer
    run 8000 app