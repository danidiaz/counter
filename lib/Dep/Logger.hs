{-# LANGUAGE DeriveGeneric #-}
-- | Interface for a logger component.
module Dep.Logger (Logger (..), Message, LogLevel (..)) where

import GHC.Generics

type Message = String

newtype Logger m = Logger {log :: LogLevel -> String -> m ()}

data LogLevel
  = Trace
  | Debug
  | Info
  | Warn
  | Error
  | Fatal
  deriving (Eq, Show, Ord, Generic)
