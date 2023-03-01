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
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | A bunch of helpers for turning functions from the model (which should be
-- innocent from any web framework) into Servant handlers.
--
-- Typeclasses in this module are parameterized by a phantom type @mark@ defined
-- by users of the module, which identifies the API / model pair and helps avoid
-- instance collisions.
--
-- Clients need to define the necessary 'Convertible' instances for types in their
-- model.
--
-- Then, when writing a Servant server, they should obtain a handler adapter by
-- calling 'asHandlerCall'.
module Dep.Handler
  ( -- ToHandler (toHandler),
    asHandlerCall,
    HandlerCall (..),
    Convertible (convert),
    convertConst,
    convertId,
    convertPure,
    convertCoerce,
    ToServerError,

    -- * "Dep.Server" re-exports
    RIO,
    RHandler,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Coerce
import Data.Kind
import Data.SOP
import Data.SOP.NP (sequence_NP, trans_NP)
import Data.SOP.NS (cmap_NS, collapse_NS, sequence'_NS)
import Data.Typeable (Typeable)
import Dep.Has
import Dep.Server
import Multicurryable
import Servant.Server
  ( Handler (..),
    ServerError,
  )

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
    env
    (methodArgs :: [Type])
    (methodErrors :: [Type])
    methodSuccess
    methodResult
    method
    (handlerArgs :: [Type])
    handlerResult
    handler
    | method -> methodArgs methodResult,
      methodResult -> methodErrors methodSuccess,
      handler -> handlerArgs handlerResult
  where
  toHandler :: mark (RIO env) -> method -> handler

instance
  ( Multicurryable Either methodErrors methodSuccess methodResult,
    Multicurryable (->) methodArgs (ReaderT env IO methodResult) method,
    Multicurryable (->) handlerArgs (ReaderT env (ExceptT ServerError IO) handlerResult) handler,
    --
    AllZip (Convertible mark) handlerArgs methodArgs,
    All (ToServerError mark) methodErrors,
    Convertible mark methodSuccess handlerResult
  ) =>
  ToHandler
    mark
    env
    (methodArgs :: [Type])
    (methodErrors :: [Type])
    methodSuccess
    methodResult
    method
    (handlerArgs :: [Type])
    handlerResult
    handler
  where
  toHandler mark method =
    multicurry @(->) @handlerArgs $ \handlerArgs ->
      let methodArgs :: RIO env (NP I methodArgs)
          methodArgs =
            sequence_NP $
              trans_NP
                (Proxy @(Convertible mark))
                (\(I x) -> convert mark x)
                handlerArgs
          uncurriedMethod = multiuncurry @(->) @methodArgs method
          methodTip :: RIO env (Either (NS I methodErrors) methodSuccess)
          methodTip = do
            args <- methodArgs
            multiuncurry @Either @methodErrors @methodSuccess <$> uncurriedMethod args
          transformErrors :: NS I methodErrors -> RIO env ServerError
          transformErrors errors = do
            mappedErrors :: NS (K ServerError) methodErrors <-
              sequence'_NS $
                cmap_NS
                  (Proxy @(ToServerError mark))
                  (\(I e) -> Comp $ K <$> convert mark e)
                  errors
            pure $ collapse_NS mappedErrors
          transformSuccess :: methodSuccess -> RIO env handlerResult
          transformSuccess = convert mark
          transformMonad :: forall x. RIO env (Either ServerError x) -> ReaderT env (ExceptT ServerError IO) x
          transformMonad = coerce
          handlerTip :: ReaderT env (ExceptT ServerError IO) handlerResult
          handlerTip = transformMonad do
            tip <- methodTip
            case tip of
              Left errors -> Left <$> transformErrors errors
              Right s -> Right <$> transformSuccess s
       in handlerTip

type Convertible :: ((Type -> Type) -> Type) -> Type -> Type -> Constraint
class (Typeable source, Typeable target) => Convertible mark source target where
  convert :: forall m. Monad m => mark m -> source -> m target

convertPure :: Applicative m => (source -> target) -> r m -> source -> m target
convertPure f _ source = pure (f source)

convertConst :: Applicative m => target -> r m -> source -> m target
convertConst t _ _ = pure t

convertId :: Applicative m => r m -> source -> m source
convertId _ x = pure x

convertCoerce :: (Applicative m, Coercible source target) => deps -> source -> m target
convertCoerce _ x = pure (coerce x)

-- | A class synonym for @Convertible mark m deps source ServerError@.
class Convertible mark source ServerError => ToServerError mark source

instance Convertible mark source ServerError => ToServerError mark source

-- | Given a converter and a dependency injection environment, produce a
-- function that is able to convert \"methods\" of components from the
-- model into Servant handlers, provided the necessary 'Convert' instances exist
-- for the arguments, result values, and potential errors.
--
-- 'asHandlerCall' is in the spirit of the 'Dep.Has.asCall' invocation helper, 
-- with the extra responsability of transforming the methods it \"looks up\" 
-- into Servant handlers.
asHandlerCall ::
  forall conv deps env.
  conv (RIO env) ->
  deps ->
  HandlerCall conv deps env 
asHandlerCall conv (Call φ) =
  HandlerCall \g -> toHandler conv (φ g)

newtype HandlerCall conv deps env
  = HandlerCall
      ( forall
          r
          (methodArgs :: [Type])
          (methodErrors :: [Type])
          methodSuccess
          methodResult
          method
          (handlerArgs :: [Type])
          handlerResult
          handler.
        ( ToHandler
            conv
            env
            (methodArgs :: [Type])
            (methodErrors :: [Type])
            methodSuccess
            methodResult
            method
            (handlerArgs :: [Type])
            handlerResult
            handler,
          Has r (RIO env) deps
        ) =>
        (r (RIO env) -> method) ->
        handler
      )