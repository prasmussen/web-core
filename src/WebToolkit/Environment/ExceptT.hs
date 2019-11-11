module WebToolkit.Environment.ExceptT
    ( lookup
    ) where


import Data.Function ((&))
import Prelude hiding (lookup)
import WebToolkit.ExceptT (ExceptT)

import qualified WebToolkit.Environment as Environment
import qualified WebToolkit.ExceptT as ExceptT


lookup :: Read a => String -> ExceptT Environment.LookupError IO a
lookup key =
    Environment.Key key
        & Environment.lookup
        & ExceptT.runEither
