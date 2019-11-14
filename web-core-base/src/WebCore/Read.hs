module WebCore.Read
    ( read
    , readString
    , readText
    , readBool
    ) where


import Prelude hiding (read)

import qualified Data.Text as T
import qualified Text.Read as Read



read :: Read b => (b -> a) -> String -> [(a, String)]
read constructor str =
    case Read.readMaybe str of
        Just value ->
            [(constructor value, "")]

        Nothing ->
            []


readString :: (String -> a) -> String -> [(a, String)]
readString fromString str =
    [(fromString str, "")]


readText :: (T.Text -> a) -> String -> [(a, String)]
readText constructor str =
    [(constructor $ T.pack str, "")]


readBool :: (Bool -> a) -> String -> [(a, String)]
readBool constructor str =
    case str of
        "true" ->
            [(constructor True, "")]

        "false" ->
            [(constructor False, "")]

        "1" ->
            [(constructor True, "")]

        "0" ->
            [(constructor False, "")]

        _ ->
            []
