module Dep.Phases ((>>=), (>>)) where

import Data.Functor.Compose
import Prelude (Functor, (<$>), (<$))

(>>=) :: Functor f => f x -> (x -> g y) -> Compose f g y
f >>= k = Compose (k <$> f)

(>>) :: Functor f => f x -> g y -> Compose f g y
f >> g = Compose (g <$ f)

