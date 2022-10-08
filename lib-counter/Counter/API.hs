{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Counter.API where

import Data.Text
import Data.UUID
import Servant.API
import Servant.API.Generic (Generic, GenericMode (type (:-)))

type API = BasicAuth "foo-realm" User :> "counter" :> NamedRoutes CountersAPI

data CountersAPI mode = CountersAPI
  { counters :: mode :- Capture "counterId" CounterId :> NamedRoutes CounterAPI,
    create :: mode :- Post '[JSON] CounterId
  }
  deriving stock (Generic)

data CounterAPI mode = CounterAPI
  { increase :: mode :- "increase" :> Post '[JSON] (),
    query :: mode :- Get '[JSON] Int,
    delete :: mode :- Delete '[JSON] ()
  }
  deriving stock (Generic)

type CounterId = UUID

type Counter = Int

newtype User = User {userName :: Text}
  deriving (Eq, Show)
