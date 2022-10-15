{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Servant.Server.HandlerContext where

import Control.Lens
import GHC.TypeLits
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Typeable
import GHC.Generics qualified as G

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
  DetermineCase (ReaderT env m a) = 'AtTheTip
  DetermineCase (a -> b) = 'Function
  DetermineCase _ = 'NamedRoutes

class AddHandlerContext server where
  addHandlerContext :: HandlerContext -> server -> server

class AddHandlerContext' (c :: Cases) server where
  addHandlerContext' :: HandlerContext -> server -> server

instance AddHandlerContext' (DetermineCase server) server
    => AddHandlerContext server where
  addHandlerContext = addHandlerContext' @(DetermineCase server)

instance (HasHandlerContext env, Monad m) 
    => AddHandlerContext' 'AtTheTip (ReaderT env m a) where
  addHandlerContext' context = locally handlerContext (const context)

instance AddHandlerContext' (DetermineCase server) server
    => AddHandlerContext' 'Function (x -> server) where
  addHandlerContext' context = fmap (addHandlerContext' @(DetermineCase server) context)

instance (G.Generic (record_ x), AddHandlerContextNamed record_ (G.Rep (record_ x)))
    => AddHandlerContext' 'NamedRoutes (record_ x) where
  addHandlerContext' context record = G.to $ addHandlerContextName @_ @record_ context (G.from record)        

type AddHandlerContextNamed :: (Type -> Type) -> (k -> Type) -> Constraint 
class AddHandlerContextNamed (record_ :: Type -> Type) rep where 
  addHandlerContextName :: HandlerContext -> rep x -> rep x

instance AddHandlerContextNamed record_ fields 
    => AddHandlerContextNamed record_ (G.D1 x (G.C1 y fields)) where
  addHandlerContextName context (G.M1 (G.M1 rep)) 
    = G.M1 . G.M1 $ addHandlerContextName @_ @record_ @fields context rep

instance (AddHandlerContextNamed record_ left ,
        AddHandlerContextNamed record_ right) 
    => AddHandlerContextNamed record_ (left G.:*: right) where
  addHandlerContextName context (left G.:*: right) 
    = addHandlerContextName @_ @record_ @left context left
      G.:*:
      addHandlerContextName @_ @record_ @right context right

instance (Typeable record_, AddHandlerContext v, KnownSymbol fieldName) =>
    AddHandlerContextNamed record_ (G.S1 ('G.MetaSel ('Just fieldName) unpackedness strictness laziness) (G.Rec0 v)) where
    addHandlerContextName context (G.M1 (G.K1 v)) = 
        let fieldName = symbolVal (Proxy @fieldName)
            context' = (typeRep (Proxy @record_), fieldName) : context
         in G.M1 (G.K1 (addHandlerContext context' v))