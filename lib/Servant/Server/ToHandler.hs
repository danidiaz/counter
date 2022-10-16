{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Server.ToHandler (
  ToHandler (toHandler),
  ToServerError(toServerError),
  ToDTO(toDTO),
  FromDTO(fromDTO),
) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Coerce
import Data.Result
import Servant.Server
  ( Handler (..),
    ServerError,
  )

class ToHandler mark before after where
  toHandler :: before -> after

instance (FromDTO mark a' a, ToHandler mark b b') => ToHandler mark (a -> b) (a' -> b') where
  toHandler f = toHandler @mark . (f . fromDTO @mark)

instance ToHandlerResult mark a a' => ToHandler mark (ReaderT env IO a) (ReaderT env Handler a') where
  toHandler :: ToHandlerResult mark a a' => ReaderT env IO a -> ReaderT env Handler a'
  toHandler = coerce . fmap (toHandlerResult @mark @a @a')

class ToHandlerResult mark before after where
  toHandlerResult :: before -> Either ServerError after

data IsResult = IsResult | IsNotResult

type family DetectResult result :: IsResult where
  DetectResult (Result e a) = 'IsResult
  DetectResult _ = 'IsNotResult

class ToHandlerResult' mark (is :: IsResult) before after where
  toHandlerResult' :: before -> Either ServerError after

instance (ToHandlerResult' mark (DetectResult before) before after) => ToHandlerResult mark before after where
  toHandlerResult = toHandlerResult' @mark @(DetectResult before)

instance ToDTO mark a' a => ToHandlerResult' mark 'IsNotResult a a' where
  toHandlerResult' = Right . toDTO @mark

instance (ToServerError mark err, ToHandlerResult mark a a') => ToHandlerResult' mark 'IsResult (Result err a) a' where
  toHandlerResult' = \case
    Error err -> Left (toServerError @mark err)
    Ok r -> toHandlerResult @mark r

class ToServerError mark x where
  toServerError :: x -> ServerError

class ToDTO mark dto model | mark dto -> model where
  toDTO :: model -> dto

class FromDTO mark dto model | mark dto -> model where
  fromDTO :: dto -> model
