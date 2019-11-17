{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module WebCore.Password
    ( Plaintext
    , Hash(..)
    , HashCost
    , UnsafePlaintext
    , fromText
    , hash
    , isValid
    , fromUnsafe
    , toUnsafe
    ) where


import Data.Function ((&))

import qualified Crypto.KDF.BCrypt as Bcrypt
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GHC.Generics as GHC
import qualified WebCore.Read as Read



-- PASSWORD HASH COST


newtype HashCost = HashCost Int
    deriving (Show)


instance Read HashCost where
    readsPrec _ str =
        Read.read HashCost str



-- PLAINTEXT


newtype Plaintext = Plaintext T.Text
    deriving (Eq, GHC.Generic, Aeson.FromJSON)


instance Show Plaintext where
    show (Plaintext _) = "<REDACTED>"


fromText :: T.Text -> Plaintext
fromText password =
    Plaintext password



-- HASH

newtype Hash = Hash T.Text
    deriving (Show, GHC.Generic)

instance Aeson.ToJSON Hash where
    toJSON _ =
        Aeson.String ""


hash :: HashCost -> Plaintext -> IO Hash
hash (HashCost cost) (Plaintext plaintext) = do
    Bcrypt.hashPassword cost (TE.encodeUtf8 plaintext)
        & fmap TE.decodeUtf8
        & fmap Hash


isValid :: Plaintext -> Hash -> Bool
isValid (Plaintext plaintext) (Hash text) =
    Bcrypt.validatePassword (TE.encodeUtf8 plaintext) (TE.encodeUtf8 text)



-- UNSAFE PLAINTEXT


-- This type is unsafe because it has a ToJSON instance
newtype UnsafePlaintext = UnsafePlaintext T.Text
    deriving (GHC.Generic, Aeson.FromJSON, Aeson.ToJSON)


fromUnsafe :: UnsafePlaintext -> Plaintext
fromUnsafe (UnsafePlaintext password) =
    Plaintext password


toUnsafe :: Plaintext -> UnsafePlaintext
toUnsafe (Plaintext password) =
    UnsafePlaintext password
