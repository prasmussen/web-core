module WebCore.Data.Maybe
    ( fromBool
    , module Data.Maybe
    ) where


import Data.Maybe


fromBool :: a -> Bool -> Maybe a
fromBool value bool =
    if bool then
        Just value

    else
        Nothing
