{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module WebCore.Squeal.Password
    ( Hash(..)
    , hash
    , isValid
    , module Password
    ) where


import Data.Function ((&))
import Squeal.PostgreSQL
import WebCore.Password as Password hiding (Hash, hash, isValid)

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import qualified WebCore.BinaryParser as BinaryParser
import qualified WebCore.Password as Password
import qualified WebCore.Read as Read



newtype Hash = Hash T.Text
    deriving (Show, GHC.Generic)


instance Aeson.ToJSON Hash


instance SOP.Generic Hash
instance SOP.HasDatatypeInfo Hash

type instance PG Hash = 'PGtext


instance ToParam Hash 'PGtext where
    toParam (Hash text) =
        toParam text


instance FromValue 'PGtext Hash where
    fromValue =
        BinaryParser.parser (Hash . TE.decodeUtf8)


hash :: HashOptions -> Plaintext -> IO (Either HashError Hash)
hash options plaintext = do
    eitherHash <- Password.hash options plaintext
    case eitherHash of
        Left err ->
            pure $ Left err

        Right (Password.Hash text) ->
            pure $ Right (Hash text)


isValid :: Plaintext -> Hash -> Either HashError Bool
isValid plaintext (Hash text) =
    Password.isValid plaintext (Password.Hash text)
