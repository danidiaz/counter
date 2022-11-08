{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | A Servant API type for exposing the controls of a 'Knob'.
module Dep.Knob.API (KnobCollectionAPI (..), KnobName, KnobAPI(..)) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text
import Data.UUID
import Servant.API
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Dep.Knob (Knob)

-- type KnobCollectionAPI = Capture "knobName" KnobName :> NamedRoutes KnobAPI

data KnobCollectionAPI mode = KnobCollectionAPI
  { knobs :: mode :- Capture "knobId" KnobName :> NamedRoutes KnobAPI,
    allKnobs :: mode :- Get '[JSON] Value
  }
  deriving stock (Generic)

data KnobAPI mode = KnobAPI
  { inspectKnob :: mode :- Get '[JSON] Value,
    setKnob :: mode :- ReqBody '[JSON] Value :> PostNoContent,
    -- | Not sure if DELETE is the semantically correct way of resetting the
    -- Knob's value, but let's go with it for now.
    resetKnob :: mode :- DeleteNoContent
  }
  deriving stock (Generic)

type KnobName = Text