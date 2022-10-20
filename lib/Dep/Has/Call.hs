{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Dep.Has.Call (pattern Call) where

import Dep.Has

pattern Call :: forall env m . (forall r_ x. Has r_ m env => (r_ m -> x) -> x) -> env
pattern Call call <- (asCall -> call)
{-# COMPLETE Call #-}
{-# INLINABLE Call #-}

