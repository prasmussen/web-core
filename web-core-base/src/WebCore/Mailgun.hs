{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebCore.Mailgun
    ( Config(..)
    , Message(..)
    , Envelope
    , envelope
    , send
    ) where


import Data.Function ((&))

import qualified Data.Bifoldable as Bifoldable
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Coerce as Coerce
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Mail.Hailgun as Hailgun
import qualified WebCore.Read as Read


data Config = Config
    { domain :: Domain
    , apiKey :: ApiKey
    , sender :: Sender
    }
    deriving (Show)


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
    { subject :: T.Text
    , content :: T.Text
    , recipient :: T.Text
    }
    deriving (Show)



messageToHailgunMessage :: Config -> Message -> Either String Hailgun.HailgunMessage
messageToHailgunMessage Config{sender = Sender sender} Message{..} =
    Hailgun.hailgunMessage
        subject
        (textContent content)
        (TE.encodeUtf8 sender)
        (singleRecipient recipient) []


-- ENVELOPE


data Envelope = Envelope
    { message :: Hailgun.HailgunMessage
    , context :: Hailgun.HailgunContext
    , formatted :: T.Text
    }


instance Show Envelope where
    show Envelope{..} =
        T.unpack formatted



envelope :: Config -> Message -> Either T.Text Envelope
envelope config@Config{..} message@Message{..} = do
    msg <- messageToHailgunMessage config message
        & Bifunctor.first T.pack
    pure $ Envelope
        { message = msg
        , context =
            Hailgun.HailgunContext
                { hailgunDomain = T.unpack (Coerce.coerce domain)
                , hailgunApiKey = T.unpack (Coerce.coerce apiKey)
                , hailgunProxy = Nothing
                }
        , formatted =
            [ ("FROM", Coerce.coerce sender)
            , ("TO", recipient)
            , ("SUBJECT", subject)
            , ("CONTENT", content)
            ]
            & map Bifoldable.biList
            & map (T.intercalate ": ")
            & T.intercalate "\n"
        }



send :: Envelope -> IO (Either T.Text ())
send Envelope{..} = do
    res <- Hailgun.sendEmail context message
    pure $ res
        & Bifunctor.first Hailgun.herMessage
        & Bifunctor.first T.pack
        & Bifunctor.second (pure ())



-- HELPER FUNCTIONS


textContent :: T.Text -> Hailgun.MessageContent
textContent content =
    Hailgun.TextOnly (TE.encodeUtf8 content)



singleRecipient :: T.Text -> Hailgun.MessageRecipients
singleRecipient recipient =
    Hailgun.MessageRecipients
        { recipientsTo = [TE.encodeUtf8 recipient]
        , recipientsCC = []
        , recipientsBCC = []
        }
