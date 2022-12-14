{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Interface for a logger component.
module Dep.Logger (
  Logger (..), 
  Message, 
  LogLevel (..),
  alwaysLogFor
  ) where

import GHC.Generics
import Data.Typeable (TypeRep)
import Prelude hiding (log)

type Message = String

data Logger m = Logger {
    -- | Emit a log message.
    -- 
    -- By default equivalent to @logFor Nothing@, but might be redefined to use
    -- a 'TypeRep'.
    log :: LogLevel -> String -> m (),
    -- | Emit a log message, possibly tagged with a particular 'TypeRep'.
    logFor :: Maybe TypeRep -> LogLevel -> String -> m ()
  }

data LogLevel
  = Trace
  | Debug
  | Info
  | Warn
  | Error
  | Fatal
  deriving (Eq, Show, Ord, Generic)

alwaysLogFor :: TypeRep -> Logger m -> Logger m
alwaysLogFor tyRep Logger {logFor} = Logger {log = logFor (Just tyRep), logFor}
