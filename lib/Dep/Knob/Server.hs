{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | A Servant server for exposing the controls of a 'Knob' as REST endpoints.
module Dep.Knob.Server (knobNamed, KnobName, KnobMap, SomeKnob, KnobServer(..), makeKnobServer) where

import Control.Monad.Reader
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Coerce
import Data.Functor (($>))
import Data.Kind
import Dep.Has (Has)
import Dep.Has.Call
import Dep.Knob (Knob)
import Dep.Knob qualified
import Dep.Knob.API
import Servant (NamedRoutes)
import Servant.API (NoContent (..))
import Data.Map (Map)
import Data.Map.Strict  qualified as Map
import Servant.Server
  ( Handler,
    HasServer (ServerT),
    err404,
    err500,
  )
import Servant.Server.Generic
import Servant.Server.ToHandler
import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.Aeson.Types (parse)
import Servant (err400)

data SomeKnob m where
  SomeKnob :: forall conf m. (FromJSON conf, ToJSON conf ) => Knob conf m -> SomeKnob m

type KnobMap m = Map KnobName (SomeKnob m)

type KnobServer :: Type -> (Type -> Type) -> Type
newtype KnobServer env m = KnobServer {knobServer :: ServerT KnobCollectionAPI (ReaderT env Handler)}

knobNamed :: forall conf m . (FromJSON conf, ToJSON conf) => KnobName -> Knob conf m -> KnobMap m
knobNamed name knob = Map.singleton name (SomeKnob knob)

makeKnobServer ::
  forall env m.
  ( m ~ ReaderT env IO
  ) =>
  KnobMap m ->
  KnobServer env m
makeKnobServer knobs =
  KnobServer \knobName ->
    KnobAPI
      { inspectKnob = 
          case Map.lookup knobName knobs of
            Nothing -> throwError err404
            Just (SomeKnob knob) -> mapReaderT liftIO do
              toJSON <$> Dep.Knob.inspectKnob knob,
        setKnob = \confValue -> 
          case Map.lookup knobName knobs of
            Nothing -> throwError err404
            Just (SomeKnob knob) -> 
              case parse parseJSON confValue of
                Error _ -> throwError err400 
                Success conf -> mapReaderT liftIO do
                  Dep.Knob.setKnob knob conf $> NoContent,
        resetKnob = 
          case Map.lookup knobName knobs of
            Nothing -> throwError err404
            Just (SomeKnob knob) -> mapReaderT liftIO do
              Dep.Knob.resetKnob knob $> NoContent
      }
