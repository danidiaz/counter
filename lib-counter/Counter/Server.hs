{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Counter.Server where

import Control.Monad.IO.Class
import Counter.API
import Data.IORef
import Data.Map.Strict as Map (Map, empty)
import Data.UUID.V4
import Servant.API.BasicAuth ( BasicAuthData(BasicAuthData) )
import Servant.Server
    ( err500,
      BasicAuthCheck(BasicAuthCheck),
      BasicAuthResult(Unauthorized, Authorized),
      Context(..),
      Handler )
  
import Servant.Server.Extra
import Servant.Server.Generic (AsServerT)

makeCounterServer :: WithExistingResource Counter -> CounterAPI (AsServerT Handler)
makeCounterServer withExistingResource =
  CounterAPI
    { increase = withExistingResource (\c -> (pure (), Just (succ c))),
      query = withExistingResource (\c -> (pure c, Just c)),
      delete = withExistingResource (\_ -> (pure (), Nothing))
    }

makeCountersServer :: IORef (Map CounterId Int) -> User -> CountersAPI (AsServerT Handler)
makeCountersServer ref user =
  CountersAPI
    { counters = \counterId -> do
        makeCounterServer (handleMissing (withResourceInMap ref counterId)),
      create = do
        uuid <- liftIO nextRandom
        withResourceInMap ref uuid \case
          Nothing -> (Right uuid, Just 0)
          Just _ -> (Left err500, Nothing) -- UUID collision!
    }

makeInitialServerState :: IO (IORef (Map CounterId Int))
makeInitialServerState = newIORef Map.empty

--
-- AUTHENTICATION
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#
authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
          then return (Authorized (User "servant"))
          else return Unauthorized
   in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext
