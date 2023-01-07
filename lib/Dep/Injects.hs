{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}

module Dep.Injects where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Data.Coerce
-- import Control.Monad.Reader
-- import Control.Monad.Dep.Class

type Injects :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Constraint
class Injects r_ (m :: Type -> Type) (env :: Type) | env -> m where
    inject :: r_ m -> env




