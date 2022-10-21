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
{-# LANGUAGE NamedFieldPuns #-}
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
import Dep.Has (Has (dep))
import Dep.Has.Call
import Dep.Logger
import Dep.Logger.HandlerAware
import Dep.Repository
import Dep.Repository.Memory
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant.Server
  ( Application,
    Handler,
    HasServer (hoistServerWithContext),
    serveWithContext,
      BasicAuthCheck
  )
import Prelude hiding (log)
import Servant.Server.HandlerContext

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
    _server :: phase (ServantServer Env m)
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
    { _logger = fromBare $ noAlloc <&> \() -> Dep.Logger.HandlerAware.make,
      _counterRepository =
        fromBare $
          alloc (newIORef Map.empty) <&> \ref env@(Call call) ->
            let repo@Repository {withResource} = Dep.Repository.Memory.make ref env
             in repo
                  { withResource = \rid -> do
                      call log "Extra log message added by instrumentation"
                      withResource rid
                  },
      _getCounter = fromBare $ noAlloc <&> \() -> makeGetCounter,
      _increaseCounter = fromBare $ noAlloc <&> \() -> makeIncreaseCounter,
      _deleteCounter = fromBare $ noAlloc <&> \() -> makeDeleteCounter,
      _createCounter = fromBare $ noAlloc <&> \() -> makeCreateCounter,
      _server =
        -- Is this the best place to call 'addHandlerContext'?  It could be done
        -- im 'makeServantServer' as well.  But doing it in 'makeServantServer'
        -- would require extra constraints in the function, and it would be
        -- farther from the component which uses the HandlerContext, which is
        -- the Logger.
        fromBare $
          noAlloc <&> \() env ->
            let servantServer@(ServantServer {server}) = makeServantServer env
             in servantServer {server = addHandlerContext [] server}
    }
  where
    alloc :: IO a -> ContT () IO a
    alloc = liftIO
    noAlloc :: ContT () IO ()
    noAlloc = pure ()

-- | Boilerplate that enables components to find their own dependencies in the
-- DI context.
deriving via Autowired (Cauldron Identity m) instance Autowireable r_ m (Cauldron Identity m) => Has r_ m (Cauldron Identity m)

-- Monad used by the Servant server.
type M :: Type -> Type
type M = ReaderT Env Handler

main :: IO ()
main = do
  -- We run the allocation phase of the DI context
  runContT (pullPhase cauldron) \allocated -> do
    -- We run the wiring phase of the DI context
    let wired :: Cauldron Identity M' = fixEnv allocated
        -- We extract the server from the DI context.
        ServantServer {server} = dep wired
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