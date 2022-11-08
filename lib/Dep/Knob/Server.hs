{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
module Dep.Knob.Server (knobNamed, KnobName, KnobMap, SomeKnob, KnobServer (..), makeKnobServer) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Types (parse)
import Data.Bifunctor
import Data.Coerce
import Data.Function ((&))
import Data.Functor (($>))
import Data.Kind
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import Dep.Has (Has)
import Dep.Has.Call
import Dep.Knob (Knob)
import Dep.Knob qualified
import Dep.Knob.API
import Servant (NamedRoutes, err400)
import Servant.API (NoContent (..))
import Servant.Server
  ( Handler,
    HasServer (ServerT),
    err404,
    err500,
  )
import Servant.Server.Generic
import Servant.Server.ToHandler

data SomeKnob m where
  SomeKnob :: forall conf m. (FromJSON conf, ToJSON conf) => Knob conf m -> SomeKnob m

type KnobMap m = Map KnobName (SomeKnob m)

type KnobServer :: Type -> (Type -> Type) -> Type
newtype KnobServer env m = KnobServer {knobServer :: ServerT (NamedRoutes KnobCollectionAPI) (ReaderT env Handler)}

knobNamed :: forall conf m. (FromJSON conf, ToJSON conf) => KnobName -> Knob conf m -> KnobMap m
knobNamed name knob = Map.singleton name (SomeKnob knob)

makeKnobServer ::
  forall env m.
  ( m ~ ReaderT env IO
  ) =>
  KnobMap m ->
  KnobServer env m
makeKnobServer knobs =
  KnobServer $
    KnobCollectionAPI
      { allKnobs = do
          values <- for knobs \(SomeKnob knob) -> do
            conf <- mapReaderT liftIO $ Dep.Knob.inspectKnob knob
            pure $ toJSON conf
          pure $ values & Map.toList & fmap (first fromText) & object,
        knobs = \knobName ->
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
      }
