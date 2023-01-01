{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | This is the Servant API for counters.
--
-- It uses named routes for convenience.
--
-- Notice that it defines its own datatypes, instead of reusing datatypes from the model.
module Counter.API where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text ( Text )
import Data.UUID ( UUID )
import Servant.API
    ( FromHttpApiData,
      BasicAuth,
      Capture,
      JSON,
      GenericMode(type (:-)),
      NamedRoutes,
      type (:>),
      Delete,
      Get,
      Post )
import Servant.API.Generic (Generic)

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