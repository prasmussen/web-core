module WebToolkit.Environment
    ( Environment
    , LookupError(..)
    , Environment.getEnvironment
    , lookup
    ) where


import Data.Function ((&))
import Prelude hiding (lookup)

import qualified Data.Bifunctor as Bifunctor
import qualified Prelude
import qualified Safe
import qualified System.Environment as Environment


data LookupError
    = KeyNotFound String
    | ParseFailed ParseError
    deriving (Show)


data ParseError = ParseError
    { lookupKey :: String
    , details :: String
    } deriving (Show)



type Environment =
    [(String, String)]


lookup :: Read a => Environment -> String -> Either LookupError a
lookup environment key =
    case Prelude.lookup key environment of
        Just value ->
            value
                & Safe.readEitherSafe
                & Bifunctor.first (ParseFailed . ParseError key)

        Nothing ->
            Left (KeyNotFound key)
