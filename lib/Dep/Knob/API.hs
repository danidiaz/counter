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

module Dep.Knob.API where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.UUID
import Servant.API
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Dep.Knob (Knob)

data KnobAPI knob mode = KnobAPI
  { getKnob :: mode :- Get '[JSON] knob,
    setKnob :: mode :- ReqBody '[JSON] knob :> PostNoContent,
    resetKnob :: mode :- DeleteNoContent
  }
  deriving stock (Generic)

type family KnobAPIFor k where
    KnobAPIFor (Knob knob knobbed) = KnobAPI knob

