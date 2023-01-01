module Dep.Server (ModelMonad, HandlerMonad) where

import Control.Monad.Trans.Reader (ReaderT)
import Servant.Server (Handler)

type ModelMonad env = ReaderT env IO

type HandlerMonad env = ReaderT env Handler
