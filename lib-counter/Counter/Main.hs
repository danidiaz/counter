{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Counter.Main where

import Control.Monad.Trans.Reader
import Counter.API
import Counter.Server
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant.Server (Application, BasicAuthCheck, hoistServerWithContext, serveWithContext)
import Control.Monad.IO.Class
import Data.Functor

import Dep.Env
import Counter.Model

import GHC.Generics (Generic)
import Control.Monad.Trans.Cont
import Data.Map.Strict as Map (Map, empty)
import Data.IORef

data CompositionRoot h m = CompositionRoot {
  _getCounter :: h (GetCounter m), 
  _increaseCounter :: h (IncreaseCounter m), 
  _deleteCounter :: h (DeleteCounter m), 
  _createCounter :: h (CreateCounter m),
  _counterRepository :: h (CounterRepository m)
} deriving stock (Generic) 
  deriving anyclass (FieldsFindableByType, Phased)

type Phases m = ContT () IO `Compose` Constructor (CompositionRoot Identity m)

env :: MonadIO m => CompositionRoot (Phases m) m 
env = CompositionRoot {
  _getCounter = fromBare $ noAlloc makeGetCounter,
  _increaseCounter = fromBare $ noAlloc makeIncreaseCounter,
  _deleteCounter = fromBare $ noAlloc makeDeleteCounter,
  _createCounter = fromBare $ noAlloc makeCreateCounter,
  _counterRepository = fromBare $ 
    alloc (newIORef Map.empty) 
      <&> \ref _ -> makeInMemoryCounterRepository ref
}
  where 
    alloc :: IO a -> ContT () IO a
    alloc = liftIO
    noAlloc :: a -> ContT () IO a
    noAlloc = pure

deriving via Autowired (CompositionRoot Identity m) instance Autowireable r_ m (CompositionRoot Identity m) => Has r_ m (CompositionRoot Identity m)


main :: IO ()
main = do
  env <- makeServerEnv
  -- https://docs.servant.dev/en/stable/cookbook/hoist-server-with-context/HoistServerWithContext.html
  let server' = hoistServerWithContext (Proxy @API) (Proxy @'[BasicAuthCheck User]) (`runReaderT` env) server
      app :: Application
      app = serveWithContext (Proxy @API) basicAuthServerContext server'
  run 8000 app