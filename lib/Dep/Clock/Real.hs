{-# LANGUAGE BlockArguments #-}

module Dep.Clock.Real where

import Dep.Clock
import Control.Monad.IO.Class
import Data.Time (getCurrentTime)

make :: MonadIO m => Clock m
make = Clock do liftIO getCurrentTime
