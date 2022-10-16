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
import Data.Kind (Type)
import Data.Map.Strict as Map (empty)
import Data.Proxy
import Dep.Env
  ( Autowireable,
    Autowired (..),
    Compose (Compose),
    Constructor,
    FieldsFindableByType,
    Identity (Identity),
    Phased,
    fixEnv,
    fromBare,
    pullPhase,
  )
import Dep.Has (Has (dep), asCall)
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

-- | The dependency injection context, where all the componets are brought
-- together and wired.
--
-- DI contexts move through a series of phases while they are being build.
-- Phases are represented as 'Composition's (nestings) of applicative functors
-- that wrap each component.
data Cauldron phase m = Cauldron
  { _logger :: phase (Logger m),
    _counterRepository :: phase (Model.CounterRepository m),
    _getCounter :: phase (GetCounter m),
    _increaseCounter :: phase (IncreaseCounter m),
    _deleteCounter :: phase (DeleteCounter m),
    _createCounter :: phase (CreateCounter m),
    _server :: phase (ServantServer m)
  }
  deriving stock (Generic)
  deriving anyclass (FieldsFindableByType, Phased)

-- | An allocation phase in which components allocate some resource that they'll
-- use during operation.
--
-- Also a good place to start service threads.
type Allocator = ContT () IO

-- | We have an allocation phase followed by a "wiring" phase in which we tie
-- the knot to obtain the fully constructed DI context.
type Phases m = Allocator `Compose` Constructor (Cauldron Identity m)

-- Monad used by the model.
type M' :: Type -> Type
type M' = ReaderT Env IO

cauldron :: Cauldron (Phases M') M'
cauldron =
  Cauldron
    { _logger = fromBare $ noAlloc $ noDeps Dep.Logger.HandlerAware.make,
      _counterRepository =
        fromBare $
          alloc (newIORef Map.empty)
            <&> \ref -> Dep.Repository.Memory.make ref,
      _getCounter = fromBare $ noAlloc makeGetCounter,
      _increaseCounter = fromBare $ noAlloc makeIncreaseCounter,
      _deleteCounter = fromBare $ noAlloc makeDeleteCounter,
      _createCounter = fromBare $ noAlloc makeCreateCounter,
      _server = fromBare $ noAlloc makeServantServer
    }
  where
    alloc :: IO a -> ContT () IO a
    alloc = liftIO
    noAlloc :: a -> ContT () IO a
    noAlloc = pure
    noDeps :: x -> env -> x
    noDeps = const

-- | Boilerplate that enables components to find their own dependencies in the
-- DI context.
deriving via Autowired (Cauldron Identity m) instance Autowireable r_ m (Cauldron Identity m) => Has r_ m (Cauldron Identity m)

-- Monad used by the Servant server.
type M :: Type -> Type
type M = ReaderT Env Handler

type ServantServer :: (Type -> Type) -> Type
newtype ServantServer m = ServantServer (ServerT API M)

-- | We construct a Servant server by extracting components from the dependency
-- injection context and using them as handlers.
--
-- We need to massage the components a little because they know nothing of
-- Servant: we need to change the monad, convert model errros to
-- 'ServantError's, convert API DTOs to and from model datatypes...
makeServantServer :: (m ~ M',
    Has GetCounter m cauldron,
    Has IncreaseCounter m cauldron,
    Has DeleteCounter m cauldron,
    Has CreateCounter m cauldron
  ) => cauldron -> ServantServer m
makeServantServer (asCall -> call) = ServantServer 
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

main :: IO ()
main = do
  -- We run the allocation phase of the DI context
  runContT (pullPhase cauldron) \allocated -> do
    -- We run the wiring phase of the DI context
    let wired :: Cauldron Identity M' = fixEnv allocated
        -- We extract the server and instrument it with the handler contexts.
        -- Omitting 'addHandlerContext' here will still compile, but the log
        -- messages won't include handler names.
        ServantServer (addHandlerContext [] -> server) = dep wired
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