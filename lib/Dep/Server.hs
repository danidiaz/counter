-- | Components encapsulating Servant servers require their dependencies to live
-- in a @ReaderT@ over @IO@ monad, and their internal server values to live in a
-- @ReaderT@ over @Handler@. See "Dep.Knob.Server" for an example.
-- 
-- Why these particular monads? Why not @IO@ and @Handler@, which are simpler?
-- Alternatively, why not remain polymorphic over the monads for added
-- flexibility? When a component constructor requires a concrete monad, it
-- forces your hand for the entire dependency injection context that includes
-- the component.
-- 
-- The reason is that choosing a concrete monad helps when converting functions
-- from the model into Servant handlers (see "Dep.Handler"). So, if we are going
-- to choose, better choose a version that carries an environment, to retain
-- that bit of flexibilty.
module Dep.Server (RIO, RHandler) where

import Control.Monad.Trans.Reader (ReaderT)
import Servant.Server (Handler)

-- Type synonym for the monad that parameterizes the dependency injection
-- context, and each particular component.
type RIO env = ReaderT env IO

-- Type synonym for the monad used in the Servant handlers.
type RHandler env = ReaderT env Handler
