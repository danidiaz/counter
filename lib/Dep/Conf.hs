{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Dep.Conf where

import Data.Aeson
import Control.Arrow (Kleisli (..))
import Data.Aeson.Types qualified as A
import Data.Yaml
import Data.String (fromString)
import Control.Monad.IO.Class
import Data.Bifunctor (first)

-- | A configuration phase in which components parse their corresponding
-- sections of the global configuration file.
type Configurator = Kleisli Parser Object

noConf :: Configurator ()
noConf = pure ()

type ConfFieldName = String

underField :: FromJSON a => ConfFieldName -> Configurator a
underField fieldName =  Kleisli \o -> A.explicitParseField parseJSON o (fromString fieldName)

parseYamlFile :: MonadIO m => Configurator a -> FilePath -> m (Either ParseException a)
parseYamlFile (Kleisli objectParser) filepath = do
  decodeResult <- liftIO $ decodeFileEither @A.Value filepath
  pure case decodeResult of
    Left err -> Left err 
    Right confValue -> 
        let parser = A.withObject "configuration" objectParser
            result = parseEither parser confValue
         in first AesonException result
