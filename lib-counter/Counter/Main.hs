{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Counter.Main where

import Control.Monad.Trans.Reader
import Counter.API
import Counter.Server
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant.Server (Application, BasicAuthCheck, hoistServerWithContext, serveWithContext)

main :: IO ()
main = do
  ref <- makeInitialServerState
  -- https://docs.servant.dev/en/stable/cookbook/hoist-server-with-context/HoistServerWithContext.html
  let server = hoistServerWithContext (Proxy @API) (Proxy @'[BasicAuthCheck User]) (`runReaderT` []) (makeCountersServer ref)
      app :: Application
      app = serveWithContext (Proxy @API) basicAuthServerContext server
  run 8000 app