module WebCore.Data.Either
    ( fromBool
    , module Data.Either
    ) where


import Data.Either


fromBool :: e -> a -> Bool -> Either e a
fromBool err value bool =
    if bool then
        Right value

    else
        Left err
