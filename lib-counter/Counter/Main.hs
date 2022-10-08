{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Counter.Main where

import Counter.API
import Network.Wai.Handler.Warp (run)
import Servant.Server ( serveWithContext, Application)
import Counter.Server
import Data.Proxy

main :: IO ()
main = do
  ref <- makeInitialServerState
  let app :: Application
      app = serveWithContext (Proxy @API) basicAuthServerContext (makeCountersServer ref)
  run 8000 app