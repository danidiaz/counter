{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Lens (Lens', view, Getter, iat)
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
import Data.Result
import Servant.Server
  ( BasicAuthCheck (BasicAuthCheck),
    BasicAuthResult (Authorized, Unauthorized),
    Context (..),
    Handler (..),
    Server,
    err500,
  )
import Servant.Server.Generic (AsServerT)
import Counter.Model

newtype Env = Env
  { 
    _handlerContext :: HandlerContext
  }
  deriving (Generic)

instance HasHandlerContext Env where
  handlerContext f s =
    getField  @"_handlerContext" s & f <&> \a -> s {_handlerContext = a}

type M :: Type -> Type
type M = ReaderT Env Handler

type M' :: Type -> Type
type M' = ReaderT Env IO


class ToHandler before after | before -> after where
  toHandler :: before -> after

instance ToHandler b b' => ToHandler (a -> b) (a -> b') where 
  toHandler = fmap toHandler 

instance ToHandlerResult a a' => ToHandler (ReaderT env IO a) (ReaderT env Handler a') where
  toHandler = coerce . fmap toHandlerResult 

class ToHandlerResult before after | before -> after where
  toHandlerResult :: before -> Either ServerError after

data IsResult = IsResult | IsNotResult

type family DetectResult result :: IsResult where
  DetectResult (Result e a) = 'IsResult
  DetectResult _ = 'IsNotResult

instance (ToHandlerResult' (DetectResult before) before after) => ToHandlerResult before after where
  toHandlerResult = toHandlerResult' @(DetectResult before)

class ToHandlerResult' (is :: IsResult) before after | before is -> after where
  toHandlerResult' :: before -> Either ServerError after

instance ToHandlerResult' 'IsNotResult a a where
  toHandlerResult' = Right

instance (ToServerError err, ToHandlerResult a a') => ToHandlerResult' 'IsResult (Result err a) a' where
  toHandlerResult' = \case 
    Problem err -> Left (toServerError err)
    Ok r -> toHandlerResult r

class ToServerError x where
  toServerError :: x -> ServerError

instance ToServerError Missing where
  toServerError _ = err404

instance ToServerError Collision where
  toServerError _ = err500

makeServerEnv :: IO Env
makeServerEnv = do
  _counterMap <- newIORef Map.empty
  pure
    Env
      { 
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
