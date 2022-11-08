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
{-# LANGUAGE ViewPatterns #-}

module Counter.Main where

import Control.Applicative
import Control.Arrow (Kleisli (..))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Counter.Model
import Counter.Model qualified as Data.Model
import Counter.Model qualified as Model
import Counter.Runner
import Counter.Server
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Data (Typeable)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.Functor
import Data.IORef
import Data.Kind (Type)
import Data.Map.Strict as Map (empty)
import Data.String (fromString)
import Data.Yaml
import Dep.Clock
import Dep.Clock.Real qualified
import Dep.Conf
import Dep.Env
  ( Autowireable,
    Autowired (..),
    Bare,
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
import Dep.Knob
import Dep.Knob.IORef
import Dep.Knob.Server
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
  { _clock :: phase (Clock m),
    _logger :: phase (Logger m),
    _counterRepository :: phase (Model.CounterRepository m),
    _getCounter :: phase (GetCounter m),
    _increaseCounter :: phase (IncreaseCounter m),
    _deleteCounter :: phase (DeleteCounter m),
    _createCounter :: phase (CreateCounter m),
    _server :: phase (ServantServer Env m),
    _knobServer :: phase (KnobServer Env m),
    _runner :: phase (ServantRunner Env m)
  }
  deriving stock (Generic)
  deriving anyclass (FieldsFindableByType, Phased)

type FinalDepEnv m = DepEnv Identity m

-- | An allocation phase in which components allocate some resource that they'll
-- use during operation.
-- Typically file handles, or mutable references.
type Allocator = ContT () IO

type Accumulator m = ([ContT () m ()], KnobMap m)

-- | We have a configuration phase followed by an allocation phase followed by a
-- "wiring" phase in which we tie the knot to obtain the fully constructed DI
-- context.
type Phases m = Configurator `Compose` Allocator `Compose` AccumConstructor (Accumulator m) (FinalDepEnv m)

-- Monad used by the model.
type M :: Type -> Type
type M = ReaderT Env IO

-- | >>> :kind! Bare (Phases M (Logger M))
-- Bare (Phases M (Logger M)) :: *
depEnv :: DepEnv (Phases M) M
depEnv =
  DepEnv
    { _clock = purePhases $ noAccum $ \_ -> Dep.Clock.Real.make,
      _logger =
        fromBare $
          underField "logger" <&> \conf ->
            do
              knobRef <- Dep.Knob.IORef.alloc conf
              pure $ Dep.Knob.IORef.make conf knobRef
              <&> \knob ~(_, _ :: FinalDepEnv M) ->
                Dep.Logger.HandlerAware.make (inspectKnob knob)
                  & (,) (mempty, knobNamed "logger" knob),
      _counterRepository =
        fromBare $
          underField "repository" <&> \conf ->
            do
              knobRef <- Dep.Knob.IORef.alloc conf
              mapRef <- Dep.Repository.Memory.alloc
              pure (Dep.Knob.IORef.make conf knobRef, mapRef)
            <&> \(knob, mapRef) ~(_, deps :: FinalDepEnv M) ->
              Dep.Repository.Memory.make Data.Model.lastUpdated (inspectKnob knob) mapRef deps & \case
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
      _getCounter = purePhases $ noAccum makeGetCounter,
      _increaseCounter = purePhases $ noAccum makeIncreaseCounter,
      _deleteCounter = purePhases $ noAccum makeDeleteCounter,
      _createCounter = purePhases $ noAccum makeCreateCounter,
      _server =
        -- Is this the best place to call 'addHandlerContext'?  It could be done
        -- im 'makeServantServer' as well.  But doing it in 'makeServantServer'
        -- would require extra constraints in the function, and it would be
        -- farther from the component which uses the HandlerContext, which is
        -- the Logger.
        purePhases $
          noAccum \deps ->
            makeServantServer deps & \case
              servantServer@(ServantServer {server}) ->
                servantServer {server = addHandlerContext [] server},
      _knobServer = purePhases \ ~((_, knobs), _) -> (mempty, makeKnobServer knobs),
      _runner =
        fromBare $
          underField "runner" <&> \conf ->
            noAlloc <&> \() ->
              noAccum \deps -> makeServantRunner conf deps
    }
  where
    noAlloc :: ContT () IO ()
    noAlloc = pure ()
    purePhases :: forall r m. ((Accumulator m, FinalDepEnv m) -> (Accumulator m, r m)) -> Phases m (r m)
    purePhases f = fromBare $ noConf <&> \_ -> noAlloc <&> \() -> f
    noAccum :: forall a b w. Monoid w => (a -> b) -> (w, a) -> (w, b)
    noAccum f ~(_, a) = (mempty, f a)
    logExtraMessage :: FinalDepEnv M -> String -> forall r. Advice Top Env IO r
    logExtraMessage (Call φ) message = makeExecutionAdvice \action -> do
      φ log Debug message
      action

-- | Boilerplate that enables components to find their own dependencies in the
-- DI context.
deriving via Autowired (FinalDepEnv m) instance Autowireable r_ m (FinalDepEnv m) => Has r_ m (FinalDepEnv m)

-- | This is the 'ReaderT' environment in which the handlers run.
newtype Env = Env
  { _handlerContext :: HandlerContext
  }
  deriving (Generic)

instance HasHandlerContext Env where
  handlerContext f s =
    -- Artisanal lens.
    getField @"_handlerContext" s & f <&> \a -> s {_handlerContext = a}

--
-- move these definitions to Dep.Env if they prove useful.
type AccumConstructor (w :: Type) (env :: Type) = (->) (w, env) `Compose` (,) w `Compose` Identity

fixEnvAccum ::
  (Phased env_, Typeable env_, Typeable m, Monoid w, Typeable w) =>
  -- | Environment where each field is wrapped in a 'Constructor'
  env_ (AccumConstructor w (env_ Identity m)) m ->
  -- | Fully constructed environment, ready for use.
  (w, env_ Identity m)
fixEnvAccum env =
  let f = pullPhase <$> pullPhase env
   in fix f

--
--

main :: IO ()
main = do
  -- CONFIGURATION PHASE
  parseResult <- parseYamlFile (pullPhase depEnv) "conf.yaml"
  case parseResult of
    Left err -> print err
    Right allocators -> do
      -- ALLOCATION PHASE
      runContT (pullPhase allocators) \constructors -> do
        -- WIRING PHASE
        let ((launchers, _), wired :: FinalDepEnv M) = fixEnvAccum constructors
        -- RUNNNING THE APP
        let ServantRunner {runServer} = dep wired
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
