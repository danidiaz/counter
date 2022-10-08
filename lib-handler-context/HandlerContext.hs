{-# LANGUAGE RankNTypes, FlexibleInstances #-}
module HandlerContext where

import Data.Typeable

type FieldName = String

type HandlerContext = [(TypeRep, FieldName)]

class HasHandlerContext e where
    -- | A lens from the environment to the call stack.

    handlerContext :: forall f . Functor f => (HandlerContext -> f HandlerContext) -> e -> f e

-- | The trivial case, useful when 'HandlerContext' is the environment type

-- of a 'Control.Monad.Reader.ReaderT'.

instance HasHandlerContext HandlerContext where
    handlerContext = id
