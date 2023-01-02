{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A typeclass for servant servers with named routes.
-- 
-- The structure of the routes is reflected into 'HandlerContext' values, which
-- are them communicated to the 'ReaderT' environment of each handler, using
-- 'local' 
-- 
-- Handlers can use the 'HandlerContext' info to provide extra context during
-- logging.
module Servant.Server.HandlerContext (
  FieldName,
  HandlerContext,
  HasHandlerContext(handlerContext),
  AddHandlerContext(addHandlerContext)
) where

import Control.Lens ( locally, Lens')
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Typeable ( Typeable, TypeRep, Proxy(Proxy), typeRep )
import GHC.Generics qualified as G
import GHC.TypeLits ( KnownSymbol, symbolVal )

-- | Name of a named route in a route record.
type FieldName = String

-- | Identifies the current handler by the 'TypeRep' of the route record and the field name of
-- the route. 
-- 
-- It's a list because named route records can be nested. Inner records appear first.
type HandlerContext = [(TypeRep, FieldName)]

-- | Reader environments that keep track of a 'HandlerContext'.  
class HasHandlerContext e where
  -- | A lens from the environment to the call stack.
  handlerContext :: Lens' e HandlerContext

-- | The trivial case, useful when 'HandlerContext' is the environment type
-- of a 'Control.Monad.Reader.ReaderT'.
instance HasHandlerContext HandlerContext where
  handlerContext = id

-- | Extra piece of info to help avoid overlapping instances.
data Cases
  = AtTheTip
  | Function
  | NamedRoutes

type DetermineCase :: Type -> Cases
type family DetermineCase server where
  DetermineCase (ReaderT env m a) = 'AtTheTip
  DetermineCase (a -> b) = 'Function
  DetermineCase _ = 'NamedRoutes

-- | Traverse a Servant server and put the name of the current (possibly nested)
-- route in the reader environment so that it's available for logging purposes.
class AddHandlerContext server where
  addHandlerContext :: 
    HandlerContext -- accumulated route. starts empty.
    -> server 
    -> server

class AddHandlerContext' (c :: Cases) server where
  addHandlerContext' :: HandlerContext -> server -> server

instance
  AddHandlerContext' (DetermineCase server) server =>
  AddHandlerContext server
  where
  addHandlerContext = addHandlerContext' @(DetermineCase server)

instance
  (HasHandlerContext env, Monad m) =>
  AddHandlerContext' 'AtTheTip (ReaderT env m a)
  where
  addHandlerContext' context = locally handlerContext (const context)

instance
  AddHandlerContext' (DetermineCase server) server =>
  AddHandlerContext' 'Function (x -> server)
  where
  addHandlerContext' context = fmap (addHandlerContext' @(DetermineCase server) context)

instance
  (G.Generic (record_ x), AddHandlerContextNamed record_ (G.Rep (record_ x))) =>
  AddHandlerContext' 'NamedRoutes (record_ x)
  where
  addHandlerContext' context record = G.to $ addHandlerContextName @_ @record_ context (G.from record)

type AddHandlerContextNamed :: (Type -> Type) -> (k -> Type) -> Constraint
class AddHandlerContextNamed (record_ :: Type -> Type) rep where
  addHandlerContextName :: HandlerContext -> rep x -> rep x

instance
  AddHandlerContextNamed record_ fields =>
  AddHandlerContextNamed record_ (G.D1 x (G.C1 y fields))
  where
  addHandlerContextName context (G.M1 (G.M1 rep)) =
    G.M1 . G.M1 $ addHandlerContextName @_ @record_ @fields context rep

instance
  ( AddHandlerContextNamed record_ left,
    AddHandlerContextNamed record_ right
  ) =>
  AddHandlerContextNamed record_ (left G.:*: right)
  where
  addHandlerContextName context (left G.:*: right) =
    addHandlerContextName @_ @record_ @left context left
      G.:*: addHandlerContextName @_ @record_ @right context right

instance
  (Typeable record_, AddHandlerContext v, KnownSymbol fieldName) =>
  AddHandlerContextNamed record_ (G.S1 ('G.MetaSel ('Just fieldName) unpackedness strictness laziness) (G.Rec0 v))
  where
  addHandlerContextName context (G.M1 (G.K1 v)) =
    let fieldName = symbolVal (Proxy @fieldName)
        context' = (typeRep (Proxy @record_), fieldName) : context
     in G.M1 (G.K1 (addHandlerContext context' v))