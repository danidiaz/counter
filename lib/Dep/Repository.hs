{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Interface for a repository component.
-- 
-- NOTE: This is probably unnecessarily complex, with the 'RunWithResource' and
-- 'RunWithExistingResource' continuations and all that.
-- 
-- NOTE: also the allowed operations on resources are pure, which is very limiting.
-- 
-- TODO: simplify this, perhaps add actual sqlite persistence.
module Dep.Repository (
    Repository(..),
    RunWithResource(..),
    RunWithExistingResource(..),
    Missing (Missing),
) where

import Data.Result
import GHC.Generics (Generic)

data Repository rid resource m = Repository
  { withResource :: rid -> m (RunWithResource resource m),
    withExistingResource :: rid -> m (RunWithExistingResource resource m)
  } deriving stock Generic

newtype RunWithResource r m = RunWithResource
  { runWithResource :: forall b. (Maybe r -> (b, Maybe r)) -> m b
  }

newtype RunWithExistingResource r m = RunWithExistingResource
  { runWithExistingResource ::
      forall b.
      (r -> (b, Maybe r)) ->
      m (Result Missing b) -- ^ 'Missing' if the resource didn't exist.
  }

-- Error type
data Missing = Missing