module Dep.Knob.IORef where

import Dep.Knob
import Data.IORef
import Control.Monad.IO.Class

allocMake :: (MonadIO n, MonadIO m) => conf -> n (Knob conf m)
allocMake conf = do 
    ref <- liftIO $ newIORef conf
    pure $ make conf ref

make :: MonadIO m => conf -> IORef conf -> Knob conf m
make conf ref =
   Knob
      { 
        setKnob = \newConf -> liftIO $ writeIORef ref newConf,
        inspectKnob = liftIO $ readIORef ref,
        resetKnob = liftIO $ writeIORef ref conf
      }
