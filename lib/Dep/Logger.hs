-- | Interface for a logger component.
module Dep.Logger (Logger (..)) where

newtype Logger m = Logger { log :: String -> m () }