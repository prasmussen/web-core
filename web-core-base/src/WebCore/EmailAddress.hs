{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WebCore.EmailAddress
    ( EmailAddress(..)
    , toByteString
    , toText
    , fromText
    ) where


import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GHC.Generics as GHC
import qualified Text.Email.Validate as Validate



newtype EmailAddress = EmailAddress Validate.EmailAddress
    deriving (Show, GHC.Generic)


instance Read EmailAddress where
    readsPrec _ str =
        case fromString str of
            Left _ ->
                []

            Right email ->
                [(email, "")]


instance Aeson.ToJSON EmailAddress where
    toJSON emailAddress =
        Aeson.String (toText emailAddress)


instance Aeson.FromJSON EmailAddress where
    parseJSON = Aeson.withText "EmailAddress" $ \text ->
        case fromText text of
            Left _ ->
                fail "Invalid email address"

            Right email ->
                pure email


fromString :: String -> Either String EmailAddress
fromString str =
    fromText (T.pack str)


fromText :: T.Text -> Either String EmailAddress
fromText text = do
    email <- text
        & T.toLower
        & TE.encodeUtf8
        & Validate.validate
    pure $ EmailAddress email


toByteString :: EmailAddress -> BS.ByteString
toByteString (EmailAddress email) =
    Validate.toByteString email


toText :: EmailAddress -> T.Text
toText (EmailAddress email) =
    TE.decodeUtf8 (Validate.toByteString email)
