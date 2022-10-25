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

-- | A bunch of helper typeclasses to convert from application models into
-- Servant handlers.
--
-- Most typeclasses are parameterized by a phantom type @mark@ defined by users
-- of the module, which identifies the API and helps avoid instance collisions.
--
-- The basic idea is defining 'ToServerError', 'ToDTO' and 'FromDTO' for types
-- in your model, in order to be able to invoke 'toHandler'.
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

-- | Converts some monadic function into something usable as a Servant handler.
-- 
-- For this to work, the monadic function must use @ReaderT@ over @IO@ for its
-- effects. 
-- 
-- Also, the required instances of 'FromDTO' must exist for its arguments, the
-- required instances of 'ToDTO' must exist for its result value, and the
-- required instances of 'ToServerError' must exist for its errors.
class ToHandler mark before after where
  toHandler :: before -> after

instance (FromDTO mark a' a, ToHandler mark b b') => ToHandler mark (a -> b) (a' -> b') where
  toHandler f = toHandler @mark . (f . fromDTO @mark)

instance ToHandlerResult mark a a' => ToHandler mark (ReaderT env IO a) (ReaderT env Handler a') where
  toHandler :: ToHandlerResult mark a a' => ReaderT env IO a -> ReaderT env Handler a'
  toHandler = coerce . fmap (toHandlerResult @mark @a @a')

class ToHandlerResult mark before after where
  toHandlerResult :: before -> Either ServerError after

-- | Extra piece of info to help avoid overlapping instances.
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
    Err err -> Left (toServerError @mark err)
    Ok r -> toHandlerResult @mark r

-- | Convert some model error to a 'ServerError'.
class ToServerError mark x where
  toServerError :: x -> ServerError

-- | Convert a datatype from the model to a DTO.
-- Typically used for results.
class ToDTO mark dto model | mark dto -> model where
  toDTO :: model -> dto

-- | Convert a DTO into a datatype form the model.
-- Typically used for request parameters and bodies.
class FromDTO mark dto model | mark dto -> model where
  fromDTO :: dto -> model
