module Dep.Logger (Logger (..)) where

newtype Logger m = Logger { log :: String -> m () }