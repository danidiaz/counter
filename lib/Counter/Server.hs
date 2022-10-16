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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module connects the Servant API with the application's model.
--
-- In particular, it defines some instances that help map datatypes to and fro. 
module Counter.Server (
  X(X),
  Env(..),
  makeServerEnv,
  authCheck,
  basicAuthServerContext,
) where

import Counter.API
import Counter.API qualified as API
import Counter.Model
import Counter.Model qualified as Model
import Data.Coerce
import Data.Function
import Data.Functor
import GHC.Generics
import GHC.Records
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.Server
  ( BasicAuthCheck (BasicAuthCheck),
    BasicAuthResult (Authorized, Unauthorized),
    Context (..),
    err404,
    err500,
  )
import Servant.Server.HandlerContext
    ( HandlerContext, HasHandlerContext(..) )
import Servant.Server.ToHandler
    ( FromDTO(..), ToDTO(..), ToServerError(..) )


-- | This is a marker type to identify the servant API.
-- 
-- In theory, a model could be used to serve different API's, so to avoid
-- instance collisions we parameterize many helper typeclasses by 'X'.
data X = X


-- | Maps a domain-relevant error to 'ServerError'.
instance ToServerError X Model.Missing where
  toServerError _ = err404

-- | Maps a domain-relevant error to 'ServerError'.
instance ToServerError X Model.Collision where
  toServerError _ = err500

-- | DTO mapping.
instance FromDTO X API.CounterId Model.CounterId where
  fromDTO = coerce

-- | DTO mapping.
instance ToDTO X API.CounterId Model.CounterId where
  toDTO = coerce

-- | DTO mapping.
instance ToDTO X API.Counter Model.Counter where
  toDTO Model.Counter {Model.counterId, Model.counterValue} =
    API.Counter
      { API.counterId = toDTO @X counterId,
        API.counterValue = counterValue
      }

-- | DTO mapping.
instance ToDTO X () () where
  toDTO = id


-- | This is the 'ReaderT' environment in which the handlers run.
newtype Env = Env
  { _handlerContext :: HandlerContext
  }
  deriving (Generic)

instance HasHandlerContext Env where
  handlerContext f s =
    -- Artisanal lens.
    getField @"_handlerContext" s & f <&> \a -> s {_handlerContext = a}

makeServerEnv :: IO Env
makeServerEnv = do
  pure
    Env
      { 
        -- This starts empty, because the handler context is (optially) set by
        -- "addHandlerContext".
        _handlerContext = []
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
