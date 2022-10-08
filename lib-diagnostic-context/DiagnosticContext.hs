module DiagnosticContext where

import Data.Typeable

type RouteName = String

type NestedRoute = [(TypeRep, RouteName)]
