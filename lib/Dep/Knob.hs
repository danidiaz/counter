{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Dep.Knob (Knob (..)) where

import Data.Kind

type Knob :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data Knob knob knobbed m = Knob {
    resetKnob :: m (),
    setKnob :: knob -> m (),
    getKnob :: m knob,
    getKnobbed :: knobbed m
  }
