{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module HandlerContext where

import Control.Lens
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Typeable
import GHC.Generics

type FieldName = String

type HandlerContext = [(TypeRep, FieldName)]

class HasHandlerContext e where
  -- | A lens from the environment to the call stack.
  handlerContext :: forall f. Functor f => (HandlerContext -> f HandlerContext) -> e -> f e

-- | The trivial case, useful when 'HandlerContext' is the environment type

-- of a 'Control.Monad.Reader.ReaderT'.

instance HasHandlerContext HandlerContext where
  handlerContext = id

data Cases
  = AtTheTip
  | Function
  | NamedRoutes

type DetermineCase :: Type -> Cases
type family DetermineCase server where
  DetermineCase (ReaderT env m a) = AtTheTip
  DetermineCase (a -> b) = Function
  DetermineCase _ = NamedRoutes

class AddHandlerContext server where
  addHandlerContext :: HandlerContext -> server -> server

class AddHandlerContext' (c :: Cases) server where
  addHandlerContext' :: HandlerContext -> server -> server

instance AddHandlerContext' (DetermineCase server) server
    => AddHandlerContext server where
  addHandlerContext = addHandlerContext' @(DetermineCase server)

instance (HasHandlerContext env, Monad m) 
    => AddHandlerContext' AtTheTip (ReaderT env m a) where
  addHandlerContext' context = locally handlerContext (const context)

instance AddHandlerContext' (DetermineCase server) server
    => AddHandlerContext' Function (x -> server) where
  addHandlerContext' context = fmap (addHandlerContext' @(DetermineCase server) context)
