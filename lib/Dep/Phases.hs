module Dep.Phases (
    -- * Qualified do-notation for building phases
    -- $warning
    (>>=), 
    (>>)
    ) where

import Data.Functor.Compose
import Prelude (Functor, (<$>), (<$))

(>>=) :: Functor f => f x -> (x -> g y) -> Compose f g y
f >>= k = Compose (k <$> f)

(>>) :: Functor f => f x -> g y -> Compose f g y
f >> g = Compose (g <$ f)

-- $warning
-- Convenient do-notation defining nested phases wrapped in 'Compose's.
-- 
-- __BEWARE__! Despite its convenience, this do-notation lacks [many of the properties](https://wiki.haskell.org/Monad_laws#The_monad_laws_in_practice) 
-- we tend to assume when working with do-notation. In particular, it's 
-- NOT associative! This means that if we have 
--
-- @
-- Dep.Phases.do    
--    somePhase
--    someOtherPhase
--    finalPhase
-- @
--
-- we CAN'T refactor to
--
-- @
-- Dep.Phases.do    
--    Dep.Phases.do 
--      somePhase
--      someOtherPhase
--    finalPhase
-- @
--
-- It would indeed be useful (it would allow pre-packaging and sharing initial
-- phases as do-blocks) but it isn't supported.
--
-- __BEWARE__ #2! Do not use 'return' in this do-notation, as it isn't provided.
--

