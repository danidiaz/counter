{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Arrow (Kleisli (..))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Counter.Model
import Counter.Model qualified as Model
import Counter.Runner
import Counter.Server
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Function ((&))
import Data.Functor
import Data.IORef
import Data.Kind (Type)
import Data.Map.Strict as Map (empty)
import Data.String (fromString)
import Data.Yaml
import Dep.Env
  ( Autowireable,
    Autowired (..),
    Compose (Compose),
    Constructor,
    DemotableFieldNames,
    FieldsFindableByType,
    Identity (Identity),
    Phased,
    fixEnv,
    fromBare,
    mapPhaseWithFieldNames,
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
data DepEnv phase m = DepEnv
  { _loggerManager :: phase (Dep.Logger.HandlerAware.Manager m),
    _logger :: phase (Logger m),
    _counterRepository :: phase (Model.CounterRepository m),
    _getCounter :: phase (GetCounter m),
    _increaseCounter :: phase (IncreaseCounter m),
    _deleteCounter :: phase (DeleteCounter m),
    _createCounter :: phase (CreateCounter m),
    _server :: phase (ServantServer Env m),
    _runner :: phase (ServantRunner Env m)
  }
  deriving stock (Generic)
  deriving anyclass (FieldsFindableByType, DemotableFieldNames, Phased)

type FinalDepEnv m = DepEnv Identity m

-- | A configuration phase in which components parse their corresponding
-- sections of the global configuration file.
type Configurator = Kleisli Parser Value

parseConf :: FromJSON a => Configurator a
parseConf = Kleisli parseJSON

-- | An allocation phase in which components allocate some resource that they'll
-- use during operation.
--
-- Also a good place to start service threads.
type Allocator = ContT () IO

-- | We have a configuration phase followed by an allocation phase followed by a
-- "wiring" phase in which we tie the knot to obtain the fully constructed DI
-- context.
type Phases m = Configurator `Compose` Allocator `Compose` Constructor (FinalDepEnv m)

-- Monad used by the model.
type M :: Type -> Type
type M = ReaderT Env IO

depEnv :: DepEnv (Phases M) M
depEnv =
  DepEnv
    { _loggerManager =
        fromBare $
          parseConf <&> \conf ->
            Dep.Logger.HandlerAware.alloc conf <&> \ref ->
              Dep.Logger.HandlerAware.make conf ref,
      _logger = fromBare $ noConf <&> \() -> noAlloc <&> \() -> Dep.Logger.HandlerAware.logger . dep,
      _counterRepository =
        fromBare $
          noConf <&> \() ->
            Dep.Repository.Memory.alloc <&> \ref env ->
              Dep.Repository.Memory.make ref env & \case 
                -- https://twitter.com/chris__martin/status/1586066539039453185
                repo@Repository {withResource} ->
                  repo
                    { withResource =
                        withResource -- Here we instrument a single method
                          & advise (logExtraMessage env "Extra log message added by instrumentation")
                    }
                    -- Here we add additional instrumentation for all the methods in the component.
                    & adviseRecord @Top @Top (\_ -> logExtraMessage env "Applies to all methods."),
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
      _getCounter = fromBare $ noConf <&> \() -> noAlloc <&> \() -> makeGetCounter,
      _increaseCounter = fromBare $ noConf <&> \() -> noAlloc <&> \() -> makeIncreaseCounter,
      _deleteCounter = fromBare $ noConf <&> \() -> noAlloc <&> \() -> makeDeleteCounter,
      _createCounter = fromBare $ noConf <&> \() -> noAlloc <&> \() -> makeCreateCounter,
      _server =
        -- Is this the best place to call 'addHandlerContext'?  It could be done
        -- im 'makeServantServer' as well.  But doing it in 'makeServantServer'
        -- would require extra constraints in the function, and it would be
        -- farther from the component which uses the HandlerContext, which is
        -- the Logger.
        fromBare $
          noConf <&> \() ->
            noAlloc <&> \() env ->
              makeServantServer env & \case
                servantServer@(ServantServer {server}) ->
                  servantServer {server = addHandlerContext [] server},
      _runner =
        fromBare $
          parseConf <&> \conf -> noAlloc <&> \() -> makeServantRunner conf
    }
  where
    noConf :: Configurator ()
    noConf = pure ()
    noAlloc :: ContT () IO ()
    noAlloc = pure ()
    logExtraMessage :: FinalDepEnv M -> String -> forall r. Advice Top Env IO r
    logExtraMessage (Call φ) message = makeExecutionAdvice \action -> do
      φ log Debug message
      action

-- | Boilerplate that enables components to find their own dependencies in the
-- DI context.
deriving via Autowired (FinalDepEnv m) instance Autowireable r_ m FinalDepEnv => Has r_ m (FinalDepEnv m)

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
  -- CONFIGURATION PHASE
  let Kleisli (A.withObject "configuration" -> parser) =
        depEnv
          -- Make the parsers which parse the conf of each component search for
          -- that conf in the global configuration, using the field name of the
          -- component in the composition root.
          & mapPhaseWithFieldNames
            (\fieldName (Kleisli f) -> Kleisli \o -> A.explicitParseField f o (fromString fieldName))
          & pullPhase @(Kleisli Parser Object)
  Right confValue <- decodeFileEither @A.Value "conf.yaml"
  case parseEither parser confValue of
    Left err -> print err
    Right allocators -> do
      -- ALLOCATION PHASE
      runContT (pullPhase allocators) \constructors -> do
        -- WIRING PHASE
        let wired :: FinalDepEnv M = fixEnv constructors
        -- RUNNNING THE APP
        let ServantRunner {runServer} = dep wired
        runServer
          Env
            { -- This starts empty, because the handler context is (optially) set by
              -- "addHandlerContext".
              _handlerContext = []
            }