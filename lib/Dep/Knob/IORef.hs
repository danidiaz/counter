module Dep.Knob.IORef where

import Dep.Knob
import Data.IORef
import Control.Monad.IO.Class

make :: MonadIO m => conf -> IORef conf -> Knob conf m
make conf ref =
   Knob
      { 
        setKnob = \newConf -> liftIO $ writeIORef ref newConf,
        inspectKnob = liftIO $ readIORef ref,
        resetKnob = liftIO $ writeIORef ref conf
      }
