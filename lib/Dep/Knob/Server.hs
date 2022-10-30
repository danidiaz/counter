{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
module Dep.Knob.Server where

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
import Servant.Server
  ( Handler,
    HasServer (ServerT),
    err404,
    err500,
  )
import Servant.Server.Generic
import Servant.Server.ToHandler

type KnobServer :: Type -> Type -> (Type -> Type) -> Type
newtype KnobServer knob env m = KnobServer {knobServer :: ServerT (NamedRoutes (KnobAPI knob)) (ReaderT env Handler)}

makeKnobServer ::
  forall component conf env m.
  ( m ~ ReaderT env IO,
    FromJSON conf,
    ToJSON conf
  ) =>
  Knob conf component m ->
  KnobServer conf env m
makeKnobServer theKnob =
  KnobServer @conf
    KnobAPI
      { getKnobConf = mapReaderT liftIO $ Dep.Knob.getKnobConf @conf theKnob,
        setKnobConf = \conf -> mapReaderT liftIO $ Dep.Knob.setKnobConf @conf theKnob conf $> NoContent,
        resetKnob = mapReaderT liftIO $ Dep.Knob.resetKnob @conf theKnob $> NoContent
      }
