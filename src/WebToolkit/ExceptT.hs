module WebToolkit.ExceptT
    ( fromEither
    , fromMaybe
    , runEither
    , runMaybe
    , mapError
    , Trans.lift
    , module Except
    ) where


import Control.Monad.Trans.Except as Except
import Data.Function ((&))

import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Either.Combinators as Combinators



fromEither :: (Monad m) => Either e a -> Except.ExceptT e m a
fromEither =
    Except.ExceptT . pure


fromMaybe :: (Monad m) => e -> Maybe a -> Except.ExceptT e m a
fromMaybe err maybe =
    Combinators.maybeToRight err maybe
        & fromEither


runEither :: (Monad m) => m (Either e a) -> Except.ExceptT e m a
runEither action =
    Trans.lift action
        >>= fromEither


runMaybe :: (Monad m) => e -> m (Maybe a) -> Except.ExceptT e m a
runMaybe err action =
    Trans.lift action
        >>= fromMaybe err


mapError :: Monad m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
mapError =
    Except.withExceptT
