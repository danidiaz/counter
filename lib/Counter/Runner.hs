{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

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
import Data.Aeson
import GHC.Generics (Generic)
import Servant.API
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import qualified Dep.Logger.HandlerAware
import Dep.Knob
import Dep.Knob.API
import Dep.Knob.Server
import Dep.Logger (Logger)

type FullAPI = 
    API
    :<|>
    BasicAuth "bar-realm" User :> "knob" :> NamedRoutes VariousKnobsAPI 

data VariousKnobsAPI mode = VariousKnobsAPI
  { loggerKnob :: mode :- "logger" :> NamedRoutes (KnobAPIFor Dep.Logger.HandlerAware.LoggerKnob)
  }
  deriving stock (Generic)

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

newtype Conf = Conf { 
        port :: Int
    } deriving stock (Show, Generic)
      deriving anyclass FromJSON

type ServantRunner :: Type -> (Type -> Type) -> Type
newtype ServantRunner env m = ServantRunner {runServer :: env -> IO () }

makeServantRunner ::
  forall m deps env.
  ( m ~ ReaderT env IO,
    Has (ServantServer env) m deps,
    Has Dep.Logger.HandlerAware.LoggerKnob m deps
  ) =>
  Conf ->
  deps -> 
  ServantRunner env m
makeServantRunner Conf {port} deps = ServantRunner {
    runServer = \env ->
        let ServantServer {server} = dep deps
            KnobServer {knobServer} = makeKnobServer $ dep @Dep.Logger.HandlerAware.LoggerKnob deps
            fullServer = server :<|> \_ -> VariousKnobsAPI { loggerKnob = knobServer }
            hoistedServer =
                hoistServerWithContext
                    (Proxy @FullAPI)
                    (Proxy @'[BasicAuthCheck User])
                    (`runReaderT` env)
                    fullServer
            app :: Application
            app = serveWithContext (Proxy @FullAPI) basicAuthServerContext hoistedServer
         in run port app
    }