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


class Handlerizable before after | before -> after where
  handlerize :: before -> after

instance Handlerizable b b' => Handlerizable (a -> b) (a -> b') where 
  handlerize = fmap handlerize 

instance ToServerError a a' => Handlerizable (ReaderT env IO a) (ReaderT env Handler a') where
  handlerize = coerce . fmap toServerError 

class ToServerError before after | before -> after where
  toServerError :: before -> Either ServerError after

data IsResult = IsResult | IsNotResult

type family DetectResult result :: IsResult where
  DetectResult (Result e a) = 'IsResult
  DetectResult _ = 'IsNotResult

instance (ToServerError' (DetectResult before) before after) => ToServerError before after where
  toServerError = toServerError' @(DetectResult before)

class ToServerError' (is :: IsResult) before after | before is -> after where
  toServerError' :: before -> Either ServerError after

instance ToServerError' 'IsNotResult a a where
  toServerError' = Right
instance ToServerError a a' => ToServerError' 'IsResult (Result Missing a) a' where
  toServerError' = \case 
    Problem _ -> Left err404
    Ok r -> toServerError r
instance ToServerError a a' => ToServerError' 'IsResult (Result Collision a) a' where
  toServerError' = \case 
    Problem _ -> Left err500
    Ok r -> toServerError r


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
