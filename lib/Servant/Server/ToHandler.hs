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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | A bunch of helper typeclasses to convert from application models into
-- Servant handlers.
--
-- Most typeclasses are parameterized by a phantom type @mark@ defined by users
-- of the module, which identifies the API and helps avoid instance collisions.
--
-- The basic idea is defining 'ToServerError', 'ToDTO' and 'FromDTO' for types
-- in your model, in order to be able to invoke 'toHandler'.
module Servant.Server.ToHandler
  ( ToHandler (toHandler),
    Convertible (convert),
    ToServerError,
    -- ToDTO(toDTO),
    -- FromDTO(fromDTO),
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor (bimap)
import Data.Coerce
import Data.Kind
import Data.SOP
import Data.SOP.NP (trans_NP)
import Data.SOP.NS (cmap_NS, collapse_NS)
import Multicurryable
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
class
  ToHandler
    mark
    (modelArgs :: [Type])
    (modelErrors :: [Type])
    modelSuccess
    modelResult
    modelTip
    model
    (handlerArgs :: [Type])
    handlerSuccess
    handlerTip
    handler
    env
    | model -> modelArgs modelTip,
      modelTip -> modelResult env,
      modelResult -> modelErrors modelSuccess,
      handler -> handlerArgs handlerTip,
      handlerTip -> handlerSuccess env
  where
  toHandler :: model -> handler

instance
  ( Multicurryable (->) modelArgs modelTip model,
    modelTip ~ ReaderT env IO modelResult,
    Multicurryable Either modelErrors modelSuccess modelResult,
    Multicurryable (->) handlerArgs handlerTip handler,
    handlerTip ~ ReaderT env Handler handlerSuccess,
    --
    AllZip (Convertible mark) handlerArgs modelArgs,
    All (ToServerError mark) modelErrors,
    Convertible mark modelSuccess handlerSuccess
  ) =>
  ToHandler
    mark
    (modelArgs :: [Type])
    (modelErrors :: [Type])
    modelSuccess
    modelResult
    modelTip
    model
    (handlerArgs :: [Type])
    handlerSuccess
    handlerTip
    handler
    env
  where
  toHandler model =
    multicurry @(->) @handlerArgs @handlerTip $ \handlerArgs ->
      let modelArgs :: NP I modelArgs
          modelArgs =
            trans_NP
              (Proxy @(Convertible mark))
              (\(I x) -> I (convert @mark x))
              handlerArgs
          uncurriedModel = multiuncurry @(->) @modelArgs @modelTip model
          modelTip :: ReaderT env IO (Either (NS I modelErrors) modelSuccess)
          modelTip = multiuncurry @Either @modelErrors @modelSuccess <$> uncurriedModel modelArgs
          transformErrors :: NS I modelErrors -> ServerError
          transformErrors errors =
            collapse_NS $
              cmap_NS
                (Proxy @(ToServerError mark))
                (\(I e) -> K (convert @mark e))
                errors
          transformSuccess :: modelSuccess -> handlerSuccess
          transformSuccess = convert @mark @modelSuccess @handlerSuccess
          handlerTip :: ReaderT env Handler handlerSuccess
          handlerTip = coerce $ bimap transformErrors transformSuccess <$> modelTip
       in handlerTip

class Convertible mark source target where
  convert :: source -> target

class Convertible mark source ServerError => ToServerError mark source

instance Convertible mark source ServerError => ToServerError mark source

-- instance (FromDTO mark a' a, ToHandler mark b b') => ToHandler mark (a -> b) (a' -> b') where
--   toHandler f = toHandler @mark . (f . fromDTO @mark)
--
-- instance ToHandlerResult mark a a' => ToHandler mark (ReaderT env IO a) (ReaderT env Handler a') where
--   toHandler :: ToHandlerResult mark a a' => ReaderT env IO a -> ReaderT env Handler a'
--   toHandler = coerce . fmap (toHandlerResult @mark @a @a')
--
-- class ToHandlerResult mark before after where
--   toHandlerResult :: before -> Either ServerError after
--
-- -- | Extra piece of info to help avoid overlapping instances.
-- data IsResult = IsResult | IsNotResult
--
-- type family DetectResult result :: IsResult where
--   DetectResult (Either e a) = 'IsResult
--   DetectResult _ = 'IsNotResult
--
-- class ToHandlerResult' mark (is :: IsResult) before after where
--   toHandlerResult' :: before -> Either ServerError after
--
-- instance (ToHandlerResult' mark (DetectResult before) before after) => ToHandlerResult mark before after where
--   toHandlerResult = toHandlerResult' @mark @(DetectResult before)
--
-- instance ToDTO mark a' a => ToHandlerResult' mark 'IsNotResult a a' where
--   toHandlerResult' = Right . toDTO @mark
--
-- instance (ToServerError mark err, ToHandlerResult mark a a') => ToHandlerResult' mark 'IsResult (Either err a) a' where
--   toHandlerResult' = \case
--     Left err -> Left (toServerError @mark err)
--     Right r -> toHandlerResult @mark r
--
-- -- | Convert some model error to a 'ServerError'.
-- class ToServerError mark x where
--   toServerError :: x -> ServerError
--
-- -- | Convert a datatype from the model to a DTO.
-- -- Typically used for results.
-- class ToDTO mark dto model | mark dto -> model where
--   toDTO :: model -> dto
--
-- -- | Convert a DTO into a datatype form the model.
-- -- Typically used for request parameters and bodies.
-- class FromDTO mark dto model | mark dto -> model where
--   fromDTO :: dto -> model
--