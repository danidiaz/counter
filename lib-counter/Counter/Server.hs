{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}


module Counter.Server where

import Control.Monad.IO.Class
import Counter.API
import Data.IORef
import Data.Map.Strict as Map (Map, empty)
import Data.UUID
import Data.UUID.V4
import Servant.API.BasicAuth ( BasicAuthData(BasicAuthData) )
import Servant.Server
    ( err500,
      BasicAuthCheck(BasicAuthCheck),
      BasicAuthResult(Unauthorized, Authorized),
      Context(..),
      Handler(..) ,
      Server)
  
import Servant.Server
import Servant.Server.Generic (AsServerT)
import HandlerContext
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Coerce
import Control.Monad.Trans.Except
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Trans.Except
import Data.Map.Strict qualified as Map
import Data.Tuple (swap)
import Dep.Has
import Dep.Env

data Env = Env {
  counterMap :: IORef (Map CounterId Int),
  _handlerContext :: HandlerContext 
}

type M :: Type -> Type
type M = ReaderT HandlerContext Handler
type M' :: Type -> Type
type M' = ReaderT HandlerContext IO 

-- https://discourse.haskell.org/t/haskell-mini-idiom-constraining-coerce/3814
type family Resultify t where
  Resultify (a -> b) = a -> Resultify b
  Resultify (ReaderT env Handler a) = ReaderT env IO (Either ServerError a)

resultify :: Coercible (Resultify handler) handler => Resultify handler -> handler
resultify = coerce

makeServer :: IORef (Map CounterId Int) -> ServerT API M
makeServer ref = do
  \user -> makeCountersServer ref user

makeCounterServer :: WithExistingResource Counter M' -> CounterAPI (AsServerT M)
makeCounterServer WithExistingResource {runWithExistingResource} =
   CounterAPI
        { increase = resultify $ runWithExistingResource (\c -> (pure (), Just (succ c))),
          query = resultify $ runWithExistingResource (\c -> (pure c, Just c)),
          delete = resultify $ runWithExistingResource  (\_ -> (pure (), Nothing))
        }

makeCountersServer :: IORef (Map CounterId Int) -> User -> CountersAPI (AsServerT M)
makeCountersServer ref user =
    CountersAPI
      { counters = \counterId -> do
          makeCounterServer (handleMissing (withResourceInMap ref counterId)),
        create = resultify do
          uuid <- liftIO nextRandom
          runWithResource (withResourceInMap ref uuid) \case
            Nothing -> (Right uuid, Just 0)
            Just _ -> (Left err500, Nothing) -- UUID collision!
      }

makeInitialServerState :: IO (IORef (Map CounterId Int))
makeInitialServerState = newIORef Map.empty

makeServerEnv :: IO Env
makeServerEnv = do
  counterMap <- newIORef Map.empty
  pure Env {
    counterMap,
    _handlerContext = []
  }


--
-- AUTHENTICATION
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#
authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
          then return (Authorized (User "servant"))
          else return Unauthorized
   in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

--
-- SOME API-GENERIC HELPERS
-- Not really related to named routes.

-- The callback receives a resource if it exists, and returns a result or an
-- error, along with a 'Nothing' if the resource should be deleted, or a 'Just'
-- if the resource should be updated.
newtype WithResource r m = WithResource { 
  runWithResource :: forall b . (Maybe r -> (b, Maybe r)) -> m b 
}

-- Like 'WithResource' but we assume the resource exists.
newtype WithExistingResource r m = WithExistingResource { 
  runWithExistingResource :: 
    forall b . (r -> (Either ServerError b, Maybe r)) -> m (Either ServerError b)
}

-- | Takes care of checking if the resource exists, throwing 404 if it doesn't.
handleMissing :: MonadIO m => WithResource r m -> WithExistingResource r m
handleMissing WithResource { runWithResource } = WithExistingResource \callback ->
  runWithResource
    \mx -> case mx of
      Nothing -> (Left err404, Nothing)
      Just x -> callback x

withResourceInMap :: (MonadIO m , Ord k) => IORef (Map k r) -> k -> WithResource r m
withResourceInMap ref k = WithResource \callback -> 
    liftIO do atomicModifyIORef' ref (swap . Map.alterF  callback k)

