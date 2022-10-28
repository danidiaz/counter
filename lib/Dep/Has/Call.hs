{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Dep.Has.Call (pattern Call, pattern Dep) where

import Dep.Has

-- | 
pattern Call :: forall env m . (forall r_ x. Has r_ m env => (r_ m -> x) -> x) -> env
pattern Call call <- (asCall -> call)
{-# COMPLETE Call #-}
{-# INLINABLE Call #-}

-- | The @δ@ should be the first argument of the functions we want to invoke.
pattern Dep :: forall env m . (forall r_ . Has r_ m env => r_ m) -> env
pattern Dep δ <- ((dep :: env -> forall r_ . Has r_ m env => r_ m) -> δ)
{-# COMPLETE Dep #-}
{-# INLINABLE Dep #-}
