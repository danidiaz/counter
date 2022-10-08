{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Servant.Server.Extra where

import Control.Monad.Except
import Data.IORef
import Data.Map.Strict as Map (Map, alterF)
import Data.Tuple (swap)
import Servant
  ( Handler,
    ServerError,
    err404
  )

--
-- SOME API-GENERIC HELPERS
-- Not really related to named routes.

-- The callback receives a resource if it exists, and returns a result or an
-- error, along with a 'Nothing' if the resource should be deleted, or a 'Just'
-- if the resource should be updated.
type WithResource r = forall b. (Maybe r -> (Either ServerError b, Maybe r)) -> Handler b

-- Like 'WithResource' but we assume the resource exists.
type WithExistingResource r = forall b. (r -> (Either ServerError b, Maybe r)) -> Handler b

-- | Takes care of checking if the resource exists, throwing 404 if it doesn't.
handleMissing :: WithResource r -> WithExistingResource r
handleMissing mightNotExist callback =
  mightNotExist
    \mx -> case mx of
      Nothing -> (Left err404, Nothing)
      Just x -> callback x

withResourceInMap :: Ord k => IORef (Map k r) -> k -> WithResource r
withResourceInMap ref k callback =
  do
    r <- liftIO do atomicModifyIORef' ref (swap . Map.alterF callback k)
    liftEither r
