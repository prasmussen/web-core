module WebToolkit.Environment
    ( Key(..)
    , LookupError(..)
    , lookup
    ) where


import Data.Function ((&))
import Prelude hiding (lookup)

import qualified Data.Bifunctor as Bifunctor
import qualified Safe
import qualified System.Environment as Environment


data LookupError
    = KeyNotFound Key
    | ParseFailed Key String
    deriving (Show)


newtype Key
    = Key String
    deriving (Show)


lookup :: Read a => Key -> IO (Either LookupError a)
lookup key@(Key keyString) = do
    maybeValue <- Environment.lookupEnv keyString
    case maybeValue of
        Just str ->
            str
                & Safe.readEitherSafe
                & Bifunctor.first (ParseFailed key)
                & pure

        Nothing ->
            key
                & KeyNotFound
                & Left
                & pure
