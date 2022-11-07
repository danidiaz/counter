{-# LANGUAGE StandaloneKindSignatures #-}
module Dep.Clock (Clock (..)) where

import Data.Kind ( Type )
import Data.Time (UTCTime)

type Clock :: (Type -> Type) -> Type
newtype Clock m = Clock {
    getNow :: m UTCTime
  }
