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

data Repository rid resource m = Repository
  { withResource :: rid -> m (RunWithResource resource m),
    withExistingResource :: rid -> m (RunWithExistingResource resource m)
  }

-- | Work with a resource in the repository.
--
-- The argument function receives @Nothing@ if the resource doesn't exist,
-- or @Just theResource@ if it does.
--
-- The argument function returns a result @b@ along with the new state of the
-- resource. If we the new state is @Nothing@, the resource will be deleted.
newtype RunWithResource r m = RunWithResource
  { runWithResource :: forall b. (Maybe r -> (b, Maybe r)) -> m b
  }

-- Here the argument function doesn't need to consider the case when the
-- resource is missing, that's already handled by 
-- 'runWithExistingResource' itself.
newtype RunWithExistingResource r m = RunWithExistingResource
  { runWithExistingResource ::
      forall b.
      (r -> (b, Maybe r)) ->
      m (Result Missing b) -- ^ 'Missing' if the resource didn't exist.
  }

-- Error type
data Missing = Missing