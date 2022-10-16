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

module Dep.Repository (
    Repository(..),
    RunWithResource(..),
    RunWithExistingResource(..),
    Missing (Missing),
) where

import Data.Result

data Repository rid resource m = Repository
  { withResource :: rid -> m (RunWithResource resource m),
    withExistingResource :: rid -> m (RunWithExistingResource resource m)
  }

newtype RunWithResource r m = RunWithResource
  { runWithResource :: forall b. (Maybe r -> (b, Maybe r)) -> m b
  }

-- Here the argument function doesn't need to consider the case when the
-- resource is missing, that's handled by the implementation of
-- 'runWithExistingResource'.
newtype RunWithExistingResource r m = RunWithExistingResource
  { runWithExistingResource ::
      forall b.
      (r -> (b, Maybe r)) ->
      m (Result Missing b)
  }

data Missing = Missing