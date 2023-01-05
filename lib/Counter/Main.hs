{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
{-# LANGUAGE QualifiedDo #-}

-- | Hidden inside this module lies the dependency injection context, where all
-- the different components of the application are brought together.
module Counter.Main (main) where

import Control.Applicative
import Control.Arrow (Kleisli (..))
import Control.Lens (Lens', (%~))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Counter.Model
import Counter.Model qualified as Model
import Counter.Runner
import Counter.Server
import Data.Bifunctor (second)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.Functor
import Dep.Phases
import Data.Functor.Identity (runIdentity)
import Data.Kind (Type)
import Data.Proxy
import Data.Typeable
import Dep.Clock
import Dep.Clock.Real qualified
import Dep.Conf
-- import Dep.Env (AccumConstructor, Autowireable, Autowired (..), Compose (Compose), Constructor, FieldsFindableByType, Identity (Identity), Phased (traverseH), fixEnvAccum, fromBare, pullPhase, toBare)
import Dep.Env hiding (AccumConstructor, Constructor, constructor, fixEnv, fixEnvAccum)
import Dep.Has (Has (dep))
import Dep.Has.Call
import Dep.Knob
import Dep.Knob.IORef
import Dep.Knob.Server
import Dep.Constructor
import Dep.Logger
import Dep.Logger.HandlerAware
import Dep.ReaderAdvice
import Dep.Repository
import Dep.Repository.Memory
import Dep.Server
import GHC.Generics (Generic)
import GHC.Records
import Servant.Server.HandlerContext
import Data.Coerce
import Prelude hiding (log)
import Data.Aeson (FromJSON)

-- | The dependency injection context, where all the componets are brought
-- together and wired.
--
-- DI contexts move through a series of phases while they are being built.
-- Phases are represented as 'Composition's (nestings) of applicative functors
-- that wrap each component.
data Deps_ phase m = Deps
  { _clock :: phase (Clock m),
    _logger :: phase (Logger m),
    _counterRepository :: phase (CounterRepository m),
    _getCounter :: phase (GetCounter m),
    _increaseCounter :: phase (IncreaseCounter m),
    _deleteCounter :: phase (DeleteCounter m),
    _createCounter :: phase (CreateCounter m),
    _server :: phase (CounterServer Env m),
    _knobServer :: phase (KnobServer Env m),
    _runner :: phase (ServantRunner Env m)
  }
  deriving stock (Generic)
  deriving anyclass (FieldsFindableByType, Phased)

type Deps m = Deps_ Identity m

-- | An allocation phase in which components allocate some resource that they'll
-- use during operation.
-- Typically file handles, or mutable references.
type Allocator = ContT () IO

type Accumulator m = ([ContT () m ()], KnobMap m)

-- | We have a configuration phase followed by an allocation phase followed by a
-- "wiring" phase in which we tie the knot to obtain the fully constructed DI
-- context.
type Phases m =
  Configurator `Compose` Allocator `Compose` AccumConstructor (Accumulator m) (Deps m)

-- Monad used by the model.
type M :: Type -> Type
type M = RIO Env

-- | >>> :kind! Bare (Phases M (Logger M))
-- Bare (Phases M (Logger M)) :: *
deps_ :: Deps_ (Phases M) M
deps_ =
  Deps
    { _clock = purePhases $ _accumConstructor_ \_ -> Dep.Clock.Real.make,
      _logger = Dep.Phases.do
          conf <- underField "logger" 
          knob <- do
              knobRef <- Dep.Knob.IORef.alloc conf
              pure @Allocator $ Dep.Knob.IORef.make conf knobRef
          _accumConstructor \_ ->
            Dep.Logger.HandlerAware.make (inspectKnob knob)
              & (,) (mempty, knobNamed "logger" knob),
      _counterRepository = Dep.Phases.do
          conf <- underField "repository" 
          (knob, mapRef) <- do
              knobRef <- Dep.Knob.IORef.alloc conf
              mapRef <- Dep.Repository.Memory.alloc
              pure @Allocator (Dep.Knob.IORef.make conf knobRef, mapRef)
          _accumConstructor \deps ->
            Dep.Repository.Memory.make Model.lastUpdated (inspectKnob knob) mapRef deps & \case
              -- https://twitter.com/chris__martin/status/1586066539039453185
              (launcher, repo@Repository {withResource}) ->
                repo
                  { withResource =
                      withResource -- Here we instrument a single method
                        & advise (logExtraMessage deps "Extra log message added by instrumentation")
                  }
                  -- Here we add additional instrumentation for all the methods in the component.
                  & adviseRecord @Top @Top (\_ -> logExtraMessage deps "Applies to all methods.")
                  & (,) ([launcher], knobNamed "repository" knob),
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
      _getCounter = purePhases $ _accumConstructor_ makeGetCounter,
      _increaseCounter = purePhases $ _accumConstructor_ makeIncreaseCounter, 
      _deleteCounter = purePhases $ _accumConstructor_ makeDeleteCounter,
      _createCounter = purePhases $ _accumConstructor_ makeCreateCounter,
      _server =
        -- Is this the best place to call 'addHandlerContext'?  It could be done
        -- im 'makeServantServer' as well.  But doing it in 'makeServantServer'
        -- would require extra constraints in the function, and it would be
        -- farther from the component which uses the HandlerContext, which is
        -- the Logger.
        purePhases $
          _accumConstructor_ \deps ->
            makeCounterServer deps & \case
              server@(CounterServer {counterServer}) ->
                server {counterServer = addHandlerContext [] counterServer},
      _knobServer = purePhases $ accumConstructor_ \(_, knobs) _ -> makeKnobServer knobs,
      _runner = Dep.Phases.do
          conf <- underField "runner"
          noAlloc
          _accumConstructor_ $ makeServantRunner conf 
    }
  where
    noAlloc :: ContT () IO ()
    noAlloc = pure ()
    purePhases :: forall r m. AccumConstructor (Accumulator m) (Deps m) (r m) -> Phases m (r m)
    purePhases f = fromBare $ noConf <&> \_ -> noAlloc <&> \() -> f
    logExtraMessage :: Deps M -> String -> forall r. Advice Top Env IO r
    logExtraMessage (Call φ) message = makeExecutionAdvice \action -> do
      φ log Debug message
      action
--    confWithoutAlloc :: FromJSON a => String -> (Configurator `Compose` Allocator) a
--    confWithoutAlloc name = Dep.Phases.do
--      conf <- underField name
--      pure conf

-- | Boilerplate that enables components to find their own dependencies in the
-- DI context.
deriving via Autowired (Deps m) instance Autowireable r_ m (Deps m) => Has r_ m (Deps m)

loggerLens :: Lens' (Deps m) (Logger m)
loggerLens f s =
  -- Artisanal lens.
  getField @"_logger" s & runIdentity & f <&> \a -> s {_logger = Identity a}

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
  parseResult <- parseYamlFile (pullPhase deps_) "conf.yaml"
  case parseResult of
    Left err -> print err
    Right allocators -> do
      -- ALLOCATION PHASE
      runContT (pullPhase allocators) \constructors -> do
        -- WIRING PHASE
        let namedLoggers =
              liftAH
                (lmapAccumConstructor (\tyRep -> loggerLens %~ alwaysLogFor tyRep))
                constructors
        let ((launchers, _), deps :: Deps M) = fixEnvAccum namedLoggers
        -- RUNNNING THE APP
        let ServantRunner {runServer} = dep deps
        let initialEnv =
              Env
                { -- This starts empty, because the handler context is (optially) set by
                  -- "addHandlerContext".
                  _handlerContext = []
                }
        let runReaderContT c f = runReaderT (runContT c f)
        runReaderContT
          (sequenceA_ launchers)
          (\() -> liftIO do runServer initialEnv)
          initialEnv

-- recompose :: forall phases phases' bare. (
--   Coercible phases bare, 
--   Coercible phases' bare,
--   Bare phases ~ bare,
--   Bare phases' ~ bare
--   ) => phases -> phases'
-- recompose = fromBare @phases' . toBare @phases