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
{-# LANGUAGE InstanceSigs #-}

module Servant.Server.Handler where

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


class ToHandler mark before after | mark before -> after where
  toHandler :: before -> after

instance ToHandler mark b b' => ToHandler mark (a -> b) (a -> b') where 
  toHandler = fmap (toHandler @mark)

instance ToHandlerResult mark a a' => ToHandler mark (ReaderT env IO a) (ReaderT env Handler a') where
  toHandler :: ToHandlerResult mark a a' => ReaderT env IO a -> ReaderT env Handler a'
  toHandler = coerce . fmap (toHandlerResult @mark)

class ToHandlerResult mark before after | mark before -> after where
  toHandlerResult :: before -> Either ServerError after

data IsResult = IsResult | IsNotResult

type family DetectResult result :: IsResult where
  DetectResult (Result e a) = 'IsResult
  DetectResult _ = 'IsNotResult

class ToHandlerResult' mark (is :: IsResult) before after | mark is before -> after where
  toHandlerResult' :: before -> Either ServerError after

instance (ToHandlerResult' mark (DetectResult before) before after) => ToHandlerResult mark before after where
  toHandlerResult = toHandlerResult' @mark @(DetectResult before)


instance ToHandlerResult' mark 'IsNotResult a a where
  toHandlerResult' = Right

instance (ToServerError mark err, ToHandlerResult mark a a') => ToHandlerResult' mark 'IsResult (Result err a) a' where
  toHandlerResult' = \case 
    Error err -> Left (toServerError @mark err)
    Ok r -> toHandlerResult @mark r

class ToServerError mark x where
  toServerError :: x -> ServerError
