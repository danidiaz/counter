{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | We might want to inspect and change the configuration of some component after 
-- the application has started.
--
-- 'Knob' is a general interface that provices that. 
--
-- @makeX@ constructors for components that can be tuned at runtime return the
-- component wrapped in a 'Knob'.
module Dep.Knob (Knob (..)) where

import Data.Kind

-- | The @conf@ need not be the same type as the configuration read from file.
type Knob :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data Knob conf component m = Knob {
    resetKnob :: m (),
    setKnobConf :: conf -> m (),
    getKnobConf :: m conf,
    getKnobComponent :: component m
  }
