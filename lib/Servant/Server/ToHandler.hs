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
  ( ToHandler,
    asHandlerCall,
    Convertible (convert),
    convertConst,
    convertId,
    convertPure,
    convertCoerce,
    ToServerError,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Coerce
import Data.Kind
import Data.SOP
import Data.SOP.NP (sequence_NP, trans_NP)
import Data.SOP.NS (cmap_NS, collapse_NS, sequence'_NS)
import Dep.Has
import Dep.Has.Call
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
    deps
    (modelArgs :: [Type])
    (modelErrors :: [Type])
    modelSuccess
    modelResult
    model
    (handlerArgs :: [Type])
    handlerSuccess
    handler
    | model -> modelArgs modelResult,
      modelResult -> modelErrors modelSuccess,
      handler -> handlerArgs handlerSuccess
  where
  toHandler :: deps -> model -> handler

instance
  ( modelM ~ ReaderT env IO,
    Multicurryable (->) modelArgs (modelM modelResult) model,
    Multicurryable Either modelErrors modelSuccess modelResult,
    handlerM ~ ReaderT env Handler,
    Multicurryable (->) handlerArgs (handlerM handlerSuccess) handler,
    --
    AllZip (Convertible mark modelM deps) handlerArgs modelArgs,
    All (ToServerError mark modelM deps) modelErrors,
    Convertible mark modelM deps modelSuccess handlerSuccess
  ) =>
  ToHandler
    mark
    deps
    (modelArgs :: [Type])
    (modelErrors :: [Type])
    modelSuccess
    modelResult
    model
    (handlerArgs :: [Type])
    handlerSuccess
    handler
  where
  toHandler deps model =
    multicurry @(->) @handlerArgs $ \handlerArgs ->
      let modelArgs :: modelM (NP I modelArgs)
          modelArgs =
            sequence_NP $
              trans_NP
                (Proxy @(Convertible mark modelM deps))
                (\(I x) -> convert @mark deps x)
                handlerArgs
          uncurriedModel = multiuncurry @(->) @modelArgs model
          modelTip :: modelM (Either (NS I modelErrors) modelSuccess)
          modelTip = do
            args <- modelArgs
            multiuncurry @Either @modelErrors @modelSuccess <$> uncurriedModel args
          transformErrors :: NS I modelErrors -> modelM ServerError
          transformErrors errors = do
            mappedErrors :: NS (K ServerError) modelErrors <-
              sequence'_NS $
                cmap_NS
                  (Proxy @(ToServerError mark modelM deps))
                  (\(I e) -> Comp $ K <$> convert @mark deps e)
                  errors
            pure $ collapse_NS mappedErrors
          transformSuccess :: modelSuccess -> modelM handlerSuccess
          transformSuccess = convert @mark @modelM @deps @modelSuccess @handlerSuccess deps
          transformMonad :: forall x. modelM (Either ServerError x) -> handlerM x
          transformMonad = coerce
          handlerTip :: handlerM handlerSuccess
          handlerTip = transformMonad do
            tip <- modelTip
            case tip of
              Left errors -> Left <$> transformErrors errors
              Right s -> Right <$> transformSuccess s
       in handlerTip

class Convertible mark m deps source target where
  convert :: deps -> source -> m target

convertPure :: Applicative m => (source -> target) -> deps -> source -> m target
convertPure f _ source = pure (f source)

convertConst :: Applicative m => target -> deps -> source -> m target
convertConst t _ _ = pure t

convertId :: Applicative m => deps -> source -> m source
convertId _ x = pure x

convertCoerce :: (Applicative m, Coercible source target) => deps -> source -> m target
convertCoerce _ x = pure (coerce x)

class Convertible mark m deps source ServerError => ToServerError mark m deps source

instance Convertible mark m deps source ServerError => ToServerError mark m deps source

asHandlerCall ::
  forall mark deps env.
  deps ->
  forall
    r
    (modelArgs :: [Type])
    (modelErrors :: [Type])
    modelSuccess
    modelResult
    model
    (handlerArgs :: [Type])
    handlerSuccess
    handler.
  ( ToHandler
      mark
      deps
      (modelArgs :: [Type])
      (modelErrors :: [Type])
      modelSuccess
      modelResult
      model
      (handlerArgs :: [Type])
      handlerSuccess
      handler,
    Has r (ReaderT env IO) deps
  ) =>
  (r (ReaderT env IO) -> model) ->
  handler
asHandlerCall deps@(Call φ) g = toHandler @mark deps (φ g)
