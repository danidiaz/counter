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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A Servant server for exposing the controls of a 'Knob' as REST endpoints.
module Dep.Knob.Server
  ( knobNamed,
    KnobName,
    KnobMap,
    SomeKnob,
    KnobServer (..),
    makeKnobServer,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Types (parse)
import Data.Bifunctor
import Data.Function ((&))
import Data.Functor (($>))
import Data.Kind
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import Dep.Knob (Knob)
import Dep.Knob qualified
import Dep.Knob.API
import Dep.Server
import Servant (NamedRoutes, err400)
import Servant.API (NoContent (..))
import Servant.Server
  ( HasServer (ServerT),
    err404, ServerError,
  )
import GHC.TypeLits

type KnobServer :: (Type -> Type) -> Type
newtype KnobServer m = KnobServer {knobServer :: ServerT (NamedRoutes KnobCollectionAPI) (H m)}

data SomeKnob m where
  SomeKnob :: forall conf m. (FromJSON conf, ToJSON conf) => Knob conf m -> SomeKnob m

newtype KnobMap m = KnobMap (Map KnobName (SomeKnob m))
  deriving newtype (Semigroup, Monoid)

knobNamed :: forall conf m. (FromJSON conf, ToJSON conf) => KnobName -> Knob conf m -> KnobMap m
knobNamed name knob = KnobMap $ Map.singleton name (SomeKnob knob)

-- See also "Dep.Server".
makeKnobServer ::
  forall env n m.
  ( Monad n,
    m ~ ReaderT env n
  ) =>
  KnobMap m ->
  KnobServer m
makeKnobServer (KnobMap knobs) =
  KnobServer $
    KnobCollectionAPI
      { allKnobs = do
          values <- for knobs \(SomeKnob knob) -> do
            conf <- mapReaderT lift $ Dep.Knob.inspectKnob knob
            pure $ toJSON conf
          pure $ values & Map.toList & fmap (first fromText) & object,
        knobs = \knobName ->
          KnobAPI
            { inspectKnob =
                case Map.lookup knobName knobs of
                  Nothing -> throwError err404
                  Just (SomeKnob knob) -> mapReaderT lift do
                    toJSON <$> Dep.Knob.inspectKnob knob,
              setKnob = \confValue ->
                case Map.lookup knobName knobs of
                  Nothing -> throwError err404
                  Just (SomeKnob knob) ->
                    case parse parseJSON confValue of
                      Error _ -> throwError err400
                      Success conf -> mapReaderT lift do
                        Dep.Knob.setKnob knob conf $> NoContent,
              resetKnob =
                case Map.lookup knobName knobs of
                  Nothing -> throwError err404
                  Just (SomeKnob knob) -> mapReaderT lift do
                    Dep.Knob.resetKnob knob $> NoContent
            }
      }

type family H (m :: Type -> Type) :: Type -> Type where
  H (ReaderT env n) = ReaderT env (ExceptT ServerError n)
  H _ = TypeError ('Text "Oops")