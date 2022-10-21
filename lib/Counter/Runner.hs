{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module connects the Servant API with the application's model.
--
-- In particular, it defines some instances that help map datatypes to and fro. 
module Counter.Runner (
  authCheck,
  basicAuthServerContext,
  ServantRunner(..),
  makeServantRunner
) where

import Counter.API
import Counter.Server
import Servant.Server
    ( Application,
      BasicAuthCheck,
      HasServer(hoistServerWithContext),
      serveWithContext,
      BasicAuthCheck(BasicAuthCheck),
      BasicAuthResult(Authorized, Unauthorized),
      Context(..) )
import Servant (BasicAuthData(..))
import Data.Kind
import Network.Wai.Handler.Warp (run)
import Dep.Has
import Control.Monad.Reader
import Data.Proxy

--
-- AUTHENTICATION
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#
authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "user" && password == "password"
          then return (Authorized (User "user"))
          else return Unauthorized
   in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

type ServantRunner :: Type -> (Type -> Type) -> Type
newtype ServantRunner env m = ServantRunner {runServer :: env -> IO () }

makeServantRunner ::
  ( m ~ ReaderT renv IO,
    Has (ServantServer renv) m env
  ) =>
  env -> 
  ServantRunner renv m
makeServantRunner env = ServantRunner {
    runServer = \renv ->
        let ServantServer {server} = dep env
            hoistedServer =
                hoistServerWithContext
                    (Proxy @API)
                    (Proxy @'[BasicAuthCheck User])
                    (`runReaderT` renv)
                    server
            app :: Application
            app = serveWithContext (Proxy @API) basicAuthServerContext hoistedServer
         in run 8000 app
    }