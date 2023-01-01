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
module Dep.Handler
  ( RIO,
    RHandler,
    ToHandler,
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
import Dep.Server

-- | Converts some monadic function from the model into something usable as a
-- Servant handler.
-- 
-- For this to work, the monadic function must use @ReaderT@ over @IO@ for its
-- effects.
-- 
-- Also, the required instances of 'Convertible' must exist for its arguments
-- and result value, and the required instances of 'ToServerError' must exist
-- for its errors.
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
    handlerResult
    handler
    | model -> modelArgs modelResult,
      modelResult -> modelErrors modelSuccess,
      handler -> handlerArgs handlerResult
  where
  toHandler :: deps -> model -> handler

instance
  ( modelMonad ~ RIO env,
    Multicurryable Either modelErrors modelSuccess modelResult,
    Multicurryable (->) modelArgs (modelMonad modelResult) model,
    handlerMonad ~ RHandler env,
    Multicurryable (->) handlerArgs (handlerMonad handlerResult) handler,
    --
    AllZip (Convertible mark modelMonad deps) handlerArgs modelArgs,
    All (ToServerError mark modelMonad deps) modelErrors,
    Convertible mark modelMonad deps modelSuccess handlerResult
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
    handlerResult
    handler
  where
  toHandler deps model =
    multicurry @(->) @handlerArgs $ \handlerArgs ->
      let modelArgs :: modelMonad (NP I modelArgs)
          modelArgs =
            sequence_NP $
              trans_NP
                (Proxy @(Convertible mark modelMonad deps))
                (\(I x) -> convert @mark deps x)
                handlerArgs
          uncurriedModel = multiuncurry @(->) @modelArgs model
          modelTip :: modelMonad (Either (NS I modelErrors) modelSuccess)
          modelTip = do
            args <- modelArgs
            multiuncurry @Either @modelErrors @modelSuccess <$> uncurriedModel args
          transformErrors :: NS I modelErrors -> modelMonad ServerError
          transformErrors errors = do
            mappedErrors :: NS (K ServerError) modelErrors <-
              sequence'_NS $
                cmap_NS
                  (Proxy @(ToServerError mark modelMonad deps))
                  (\(I e) -> Comp $ K <$> convert @mark deps e)
                  errors
            pure $ collapse_NS mappedErrors
          transformSuccess :: modelSuccess -> modelMonad handlerResult
          transformSuccess = convert @mark @modelMonad @deps @modelSuccess @handlerResult deps
          transformMonad :: forall x. modelMonad (Either ServerError x) -> handlerMonad x
          transformMonad = coerce
          handlerTip :: handlerMonad handlerResult
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
    handlerResult
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
      handlerResult
      handler,
    Has r (RIO env) deps
  ) =>
  -- | An accessor for a function inside a component.
  (r (RIO env) -> model) ->
  -- | Converted handler.
  handler
asHandlerCall deps@(Call φ) g = toHandler @mark deps (φ g)
