{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebCore.Mailgun
    ( Config(..)
    , Message(..)
    , Error(..)
    , prepareRequest
    , send
    , formatMessage
    ) where


import Data.Function ((&))

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Bifoldable as Bifoldable
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GHC.Generics as GHC
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified WebCore.Read as Read


-- CONFIG

data Config = Config
    { domain :: Domain
    , apiKey :: ApiKey
    , sender :: Sender
    }
    deriving (Show)


domainText :: Config -> T.Text
domainText Config{domain = Domain domain} =
    domain


apiKeyBS :: Config -> BS.ByteString
apiKeyBS Config{apiKey = ApiKey apiKey} =
    TE.encodeUtf8 apiKey



-- DOMAIN


newtype Domain = Domain T.Text
    deriving (Show)


instance Read Domain where
    readsPrec _ str =
        Read.readText Domain str



-- API KEY


newtype ApiKey = ApiKey T.Text
    deriving (Show)


instance Read ApiKey where
    readsPrec _ str =
        Read.readText ApiKey str



-- SENDER


newtype Sender = Sender T.Text
    deriving (Show)


instance Read Sender where
    readsPrec _ str =
        Read.readText Sender str



-- MESSAGE


data Message = Message
    { from :: T.Text
    , to :: T.Text
    , subject :: T.Text
    , text :: T.Text
    }
    deriving (Show, GHC.Generic)


instance Aeson.ToJSON Message


formatMessage :: Message -> T.Text
formatMessage Message{..} =
    [ ("FROM", from)
    , ("TO", to)
    , ("SUBJECT", subject)
    , ("CONTENT", text)
    ]
    & map Bifoldable.biList
    & map (T.intercalate ": ")
    & T.intercalate "\n"



-- REQUEST


prepareRequest :: Config -> Message -> Client.Request
prepareRequest config Message{..} =
    mconcat
        [ "https://api.eu.mailgun.net/v3/"
        , domainText config
        , "/messages"
        ]
        & T.unpack
        & Client.parseRequest_
        & Client.setRequestCheckStatus
        & Client.applyBasicAuth "api" (apiKeyBS config)
        & Client.urlEncodedBody
            [ ("from", TE.encodeUtf8 from)
            , ("to", TE.encodeUtf8 to)
            , ("subject", TE.encodeUtf8 subject)
            , ("text", TE.encodeUtf8 text)
            ]



data Error
    = RequestException Client.HttpException
    deriving (Show)


send :: Client.Request -> IO (Either Error ())
send request = do
    manager <- TLS.newTlsManager
    result <- Client.httpLbs request manager
        & Exception.try
    case result of
        Left err ->
            pure $ Left (RequestException err)

        Right _ ->
            pure $ Right ()
