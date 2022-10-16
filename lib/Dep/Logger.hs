module Dep.Logger where

newtype Logger m = Logger { log :: String -> m () }