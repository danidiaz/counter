{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Counter.Server where

import Control.Lens (Lens', view, Getter)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Counter.API
import Data.Coerce
import Data.IORef
import Data.Kind
import Data.Map.Strict as Map (Map, empty)
import Data.Map.Strict qualified as Map
import Data.Tuple (swap)
import Data.UUID
import Data.UUID.V4
import Dep.Env
import Dep.Has
import GHC.Generics
import HandlerContext
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.Server
import GHC.Records
import Data.Functor
import Data.Function
import Servant.Server
  ( BasicAuthCheck (BasicAuthCheck),
    BasicAuthResult (Authorized, Unauthorized),
    Context (..),
    Handler (..),
    Server,
    err500,
  )
import Servant.Server.Generic (AsServerT)

data Env = Env
  { _counterMap :: IORef (Map CounterId Int),
    _handlerContext :: HandlerContext
  }
  deriving (Generic)

instance HasHandlerContext Env where
  handlerContext f s =
    getField  @"_handlerContext" s & f <&> \a -> s {_handlerContext = a}

class HasCounterMap env where
  counterMap :: Getter env (IORef (Map CounterId Int))

instance HasCounterMap Env where
  counterMap f s =
    getField  @"_counterMap" s & f <&> \a -> s {_counterMap = a}

type M :: Type -> Type
type M = ReaderT Env Handler

type M' :: Type -> Type
type M' = ReaderT Env IO

-- https://discourse.haskell.org/t/haskell-mini-idiom-constraining-coerce/3814
type family Resultify t where
  Resultify (a -> b) = a -> Resultify b
  Resultify (ReaderT env Handler a) = ReaderT env IO (Either ServerError a)

resultify :: Coercible (Resultify handler) handler => Resultify handler -> handler
resultify = coerce

server :: ServerT API M
server = addHandlerContext [] \user -> CountersAPI
    { counters = \counterId -> do
        let WithExistingResource {runWithExistingResource} =
              handleMissing (withResourceInMap counterId)
         in CounterAPI
              { increase = resultify $ runWithExistingResource (\c -> (pure (), Just (succ c))),
                query = resultify $ runWithExistingResource (\c -> (pure c, Just c)),
                delete = resultify $ runWithExistingResource (\_ -> (pure (), Nothing))
              },
      create = resultify do
        uuid <- liftIO nextRandom
        runWithResource (withResourceInMap uuid) \case
          Nothing -> (Right uuid, Just 0)
          Just _ -> (Left err500, Nothing) -- UUID collision!
    }

makeServerEnv :: IO Env
makeServerEnv = do
  _counterMap <- newIORef Map.empty
  pure
    Env
      { _counterMap,
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
newtype WithResource r m = WithResource
  { runWithResource :: forall b. (Maybe r -> (b, Maybe r)) -> m b
  }

-- Like 'WithResource' but we assume the resource exists.
newtype WithExistingResource r m = WithExistingResource
  { runWithExistingResource ::
      forall b.
      (r -> (Either ServerError b, Maybe r)) ->
      m (Either ServerError b)
  }

-- | Takes care of checking if the resource exists, throwing 404 if it doesn't.
handleMissing :: MonadIO m => WithResource r m -> WithExistingResource r m
handleMissing WithResource {runWithResource} = WithExistingResource \callback ->
  runWithResource
    \mx -> case mx of
      Nothing -> (Left err404, Nothing)
      Just x -> callback x

withResourceInMap :: 
  (HasCounterMap env, 
   HasHandlerContext env,
   MonadReader env m, 
   MonadIO m) => CounterId -> WithResource Counter m
withResourceInMap k = WithResource \callback -> do
  do context <- view handlerContext
     liftIO $ print $ "Called endpoint " ++ show context
  ref <- view counterMap
  liftIO do atomicModifyIORef' ref (swap . Map.alterF callback k)
