{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Counter.Server where

import Counter.API
import Counter.API qualified as API
import Counter.Model
import Counter.Model qualified as Model
import Data.Coerce
import Data.Function
import Data.Functor
import Data.Kind
import GHC.Generics
import GHC.Records
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Control.Monad.Trans.Reader
import Servant.Server
  ( BasicAuthCheck (BasicAuthCheck),
    BasicAuthResult (Authorized, Unauthorized),
    Context (..),
    Handler (..),
    err404,
    err500,
  )
import Servant.Server.HandlerContext
    ( HandlerContext, HasHandlerContext(..) )
import Servant.Server.ToHandler
    ( FromDTO(..), ToDTO(..), ToServerError(..) )

newtype Env = Env
  { _handlerContext :: HandlerContext
  }
  deriving (Generic)

instance HasHandlerContext Env where
  handlerContext f s =
    getField @"_handlerContext" s & f <&> \a -> s {_handlerContext = a}

type M :: Type -> Type
type M = ReaderT Env Handler

type M' :: Type -> Type
type M' = ReaderT Env IO

data X = X

instance ToServerError X Model.Missing where
  toServerError _ = err404

instance ToServerError X Model.Collision where
  toServerError _ = err500

instance FromDTO X API.CounterId Model.CounterId where
  fromDTO = coerce

instance ToDTO X API.CounterId Model.CounterId where
  toDTO = coerce

instance ToDTO X API.Counter Model.Counter where
  toDTO Model.Counter {Model.counterId, Model.counterValue} =
    API.Counter
      { API.counterId = toDTO @X counterId,
        API.counterValue = counterValue
      }

instance ToDTO X () () where
  toDTO = id

makeServerEnv :: IO Env
makeServerEnv = do
  pure
    Env
      { _handlerContext = []
      }

--
-- AUTHENTICATION
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#
authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "user" && password == "password"
          then return (Authorized (User "user"))
          else return Unauthorized
   in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext
