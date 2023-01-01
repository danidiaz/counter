module Dep.Server (ModelMonad, HandlerMonad) where

import Control.Monad.Trans.Reader (ReaderT)
import Servant.Server (Handler)

-- TODO: this isn't really a "model monad". In fact the business logic in the
-- model is completely unaware of it. What would be a better name?
type ModelMonad env = ReaderT env IO

type HandlerMonad env = ReaderT env Handler
