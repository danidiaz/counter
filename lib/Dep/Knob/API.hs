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
module Dep.Knob.API (KnobAPI(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.UUID
import Servant.API
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Dep.Knob (Knob)

data KnobAPI conf mode = KnobAPI
  { getKnobConf :: mode :- Get '[JSON] conf,
    setKnobConf :: mode :- ReqBody '[JSON] conf :> PostNoContent,
    -- | Not sure if DELETE is the semantically correct way of resetting the
    -- Knob's value, but let's go with it for now.
    resetKnob :: mode :- DeleteNoContent
  }
  deriving stock (Generic)
