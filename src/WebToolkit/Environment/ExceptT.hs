module WebToolkit.Environment.ExceptT
    ( lookup
    , module Environment
    ) where


import Data.Function ((&))
import Prelude hiding (lookup)
import WebToolkit.Environment as Environment hiding (lookup)
import WebToolkit.ExceptT (ExceptT)

import qualified WebToolkit.Environment as Environment
import qualified WebToolkit.ExceptT as ExceptT


lookup :: Read a => String -> ExceptT LookupError IO a
lookup key =
    Key key
        & Environment.lookup
        & ExceptT.runEither
