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

module Counter.Main where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Counter.Model
import Counter.Model qualified as Model
import Counter.Runner
import Counter.Server
import Data.Function ((&))
import Data.Functor
import Data.IORef
import Data.Kind (Type)
import Data.Map.Strict as Map (empty)
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
import Dep.ReaderAdvice
import Dep.Repository
import Dep.Repository.Memory
import GHC.Generics (Generic)
import GHC.Records
import Servant.Server.HandlerContext
import Prelude hiding (log)

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
    _server :: phase (ServantServer Env m),
    _runner :: phase (ServantRunner Env m)
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
          alloc (newIORef Map.empty) <&> \ref env ->
            let repo@Repository {withResource} = Dep.Repository.Memory.make ref env
             in repo
                  { withResource =
                      withResource
                        & advise (logExtraMessage env "Extra log message added by instrumentation")
                  },
      -- A less magical (compared to the Advice method above) way of adding the
      -- extra log message. Perhaps it should be preferred, but the problem is
      -- that it forces you to explicitly pass down the positional arguments of
      -- every function, which might get annoying when instrumenting lots of
      -- functions.
      --
      --          { withResource = \rid -> do
      --              call log "Extra log message added by instrumentation"
      --              withResource rid
      --          },
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
             in servantServer {server = addHandlerContext [] server},
      _runner =
        fromBare $
          noAlloc <&> \() -> makeServantRunner
    }
  where
    alloc :: IO a -> ContT () IO a
    alloc = liftIO
    noAlloc :: ContT () IO ()
    noAlloc = pure ()
    logExtraMessage :: Cauldron Identity M' -> String -> forall r. Advice Top Env IO r
    logExtraMessage (Call φ) message = makeExecutionAdvice \action -> do
      φ log message
      action

-- | Boilerplate that enables components to find their own dependencies in the
-- DI context.
deriving via Autowired (Cauldron Identity m) instance Autowireable r_ m (Cauldron Identity m) => Has r_ m (Cauldron Identity m)

-- | This is the 'ReaderT' environment in which the handlers run.
newtype Env = Env
  { _handlerContext :: HandlerContext
  }
  deriving (Generic)

instance HasHandlerContext Env where
  handlerContext f s =
    -- Artisanal lens.
    getField @"_handlerContext" s & f <&> \a -> s {_handlerContext = a}

main :: IO ()
main = do
  -- We run the allocation phase of the DI context
  runContT (pullPhase cauldron) \allocated -> do
    -- We run the wiring phase of the DI context
    let wired :: Cauldron Identity M' = fixEnv allocated
        -- We extract the server from the DI context.
        ServantRunner {runServer} = dep wired
    runServer
      Env
        { -- This starts empty, because the handler context is (optially) set by
          -- "addHandlerContext".
          _handlerContext = []
        }