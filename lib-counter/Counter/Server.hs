{-# LANGUAGE BlockArguments #-}
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

module Counter.Server where

import Control.Monad.IO.Class
import Counter.API
import Data.IORef
import Data.Map.Strict as Map (Map, empty)
import Data.UUID.V4
import Servant.API.BasicAuth ( BasicAuthData(BasicAuthData) )
import Servant.Server
    ( err500,
      BasicAuthCheck(BasicAuthCheck),
      BasicAuthResult(Unauthorized, Authorized),
      Context(..),
      Handler(..) )
  
import Servant.Server.Extra
import Servant.Server.Generic (AsServerT)
import HandlerContext
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Coerce
import Control.Monad.Trans.Except
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity

type M :: Type -> Type
type M = ReaderT HandlerContext Handler


class Tip m a n b e before after | before -> m a e, after -> n b e where
  mapTip :: (m a -> n b) -> before -> after 

-- ReaderT is not particularly special, just a convenient known
-- type constructor to recognize at the tip.
instance Tip m a n b e (ReaderT e m a) (ReaderT e n b) where
  mapTip f (ReaderT r) = ReaderT (f . r) 

instance Tip m a n b () (IdentityT m a) (IdentityT n b) where
  mapTip f (IdentityT r) = IdentityT (f r)


instance Tip m a n b e before after =>
  Tip m a n b e (x -> before) (x -> after) where
  mapTip f r = mapTip f . r 

makeCounterServer :: WithExistingResource Counter -> CounterAPI (AsServerT M)
makeCounterServer withExistingResource =
  let tipM f = mapTip (Handler . ExceptT) f
   in CounterAPI
        { increase = tipM $ withExistingResource (\c -> (pure (), Just (succ c))),
          query = tipM $ withExistingResource (\c -> (pure c, Just c)),
          delete = tipM $ withExistingResource (\_ -> (pure (), Nothing))
        }

makeCountersServer :: IORef (Map CounterId Int) -> User -> CountersAPI (AsServerT M)
makeCountersServer ref user =
  let tipM f = mapTip (Handler . ExceptT) f
   in
    CountersAPI
      { counters = \counterId -> do
          makeCounterServer (handleMissing (withResourceInMap ref counterId)),
        create = tipM do
          uuid <- liftIO nextRandom
          withResourceInMap ref uuid \case
            Nothing -> (Right uuid, Just 0)
            Just _ -> (Left err500, Nothing) -- UUID collision!
      }

makeInitialServerState :: IO (IORef (Map CounterId Int))
makeInitialServerState = newIORef Map.empty

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
