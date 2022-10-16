{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Counter.API where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.UUID
import Servant.API
import Servant.API.Generic (Generic, GenericMode (type (:-)))

type API = BasicAuth "foo-realm" User :> "counter" :> NamedRoutes CounterCollectionAPI

data CounterCollectionAPI mode = CounterCollectionAPI
  { counters :: mode :- Capture "counterId" CounterId :> NamedRoutes CounterAPI,
    create :: mode :- Post '[JSON] CounterId
  }
  deriving stock (Generic)

data CounterAPI mode = CounterAPI
  { increase :: mode :- "increase" :> Post '[JSON] (),
    query :: mode :- Get '[JSON] Counter,
    delete :: mode :- Delete '[JSON] ()
  }
  deriving stock (Generic)

newtype User = User {userName :: Text}
  deriving (Eq, Show)

newtype CounterId = CounterId UUID
  deriving stock (Ord, Eq)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

data Counter = Counter
  { counterId :: CounterId,
    counterValue :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)