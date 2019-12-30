{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module WebCore.Password
    ( Plaintext
    , Hash(..)
    , HashOptions(..)
    , HashError
    , fromText
    , hash
    , isValid
    , minLength
    ) where


import Data.Function ((&))

import qualified Crypto.Argon2 as Argon2
import qualified Crypto.Random as Random
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ShortText
import qualified Data.Word as Word
import qualified GHC.Generics as GHC
import qualified WebCore.Read as Read


-- HASH OPTIONS


data HashOptions = HashOptions
    { hashIterations :: HashIterations
    , hashMemory :: HashMemory
    , hashParallelism :: HashParallelism
    }


newtype HashIterations = HashIterations Word.Word32
    deriving (Show)


instance Read HashIterations where
    readsPrec _ str =
        Read.read HashIterations str


newtype HashMemory = HashMemory Word.Word32
    deriving (Show)


instance Read HashMemory where
    readsPrec _ str =
        Read.read HashMemory str


newtype HashParallelism = HashParallelism Word.Word32
    deriving (Show)


instance Read HashParallelism where
    readsPrec _ str =
        Read.read HashParallelism str


hashOptions :: HashOptions -> Argon2.HashOptions
hashOptions HashOptions
    { hashIterations = HashIterations iterations
    , hashMemory = HashMemory memory
    , hashParallelism = HashParallelism parallelism
    } =
    Argon2.defaultHashOptions
        { Argon2.hashIterations = iterations
        , Argon2.hashMemory = memory
        , Argon2.hashParallelism = parallelism
        , Argon2.hashVariant = Argon2.Argon2id
        }

-- PLAINTEXT


newtype Plaintext = Plaintext T.Text
    deriving (Eq, GHC.Generic)


instance Aeson.FromJSON Plaintext where
    parseJSON = Aeson.withText "Plaintext" $ \text ->
        case fromText text of
            Just plaintext ->
                pure plaintext

            Nothing ->
                fail $ "Password is too short, minimum length is " <> show minLength

instance Show Plaintext where
    show (Plaintext _) = "<REDACTED>"


minLength :: Int
minLength = 8


fromText :: T.Text -> Maybe Plaintext
fromText password =
    if T.length password < minLength then
        Nothing

    else
        Just (Plaintext password)



-- HASH

newtype Hash = Hash T.Text
    deriving (Show, GHC.Generic, Eq)

instance Aeson.ToJSON Hash where
    toJSON _ =
        Aeson.String ""


type HashError = Argon2.Argon2Status


hash :: HashOptions -> Plaintext -> IO (Either HashError Hash)
hash options (Plaintext plaintext) = do
    salt <- Random.getRandomBytes 16
    Argon2.hashEncoded (hashOptions options) (TE.encodeUtf8 plaintext) (salt)
        & fmap ShortText.toText
        & fmap Hash
        & pure


isValid :: Plaintext -> Hash -> Either HashError Bool
isValid (Plaintext plaintext) (Hash text) =
    case Argon2.verifyEncoded (ShortText.fromText text) (TE.encodeUtf8 plaintext) of
        Argon2.Argon2Ok ->
            Right True

        Argon2.Argon2VerifyMismatch ->
            Right False

        err ->
            Left err
