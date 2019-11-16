{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebCore.Logger
    ( Logger
    , Config(..)
    , Format(..)
    , Severity(..)
    , Msg(..)
    , Event(..)
    , newLogger
    , runLogger
    , log
    ) where


import Data.Function ((&))
import Prelude hiding (log)

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Monad as Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO



-- CONFIG


data Config = Config
    { format :: Format
    }
    deriving (Show)


data Format
    = Normal
    | Systemd
    deriving (Show)


instance Read Format where
    readsPrec _ value =
        case value of
            "systemd" ->
                [(Systemd, "")]

            "normal" ->
                [(Normal, "")]

            _ ->
                []


-- SEVERITY


-- Systemd log levels: https://unix.stackexchange.com/a/408420
data Severity
    = Error
    | Info
    | Debug
    deriving (Show)


severityPriority :: Severity -> Int
severityPriority severity =
    case severity of
        Error ->
            3

        Info ->
            6

        Debug ->
            7


severityText :: Severity -> T.Text
severityText severity =
    case severity of
        Error ->
            "Error"

        Info ->
            "Info"

        Debug ->
            "Debug"



-- MSG


data Msg where
    StringMsg :: String -> Msg
    TextMsg :: T.Text -> Msg
    ShowMsg :: Show a => a -> Msg


instance Show Msg where
    show msg =
        case msg of
            StringMsg str ->
                str

            TextMsg str ->
                T.unpack str

            ShowMsg value ->
                show value


msgToText :: Msg -> T.Text
msgToText msg =
    case msg of
        StringMsg str ->
            T.pack str

        TextMsg txt ->
            txt

        ShowMsg value ->
            T.pack (show value)


-- EVENT


data Event = Event
    { severity :: Severity
    , context :: [T.Text]
    , message :: [Msg]
    }
    deriving (Show)


formatEvent :: Format -> Event -> T.Text
formatEvent format Event{..} =
    case format of
        Systemd ->
            mconcat
                [ "<"
                , T.pack $ show $ severityPriority severity
                , ">"
                , "["
                , T.intercalate ", " context
                , "] "
                , message
                    & map msgToText
                    & T.intercalate " "
                ]

        Normal ->
            mconcat
                [ "<"
                , severityText severity
                , "> "
                , "["
                , T.intercalate ", " context
                , "] "
                , message
                    & map msgToText
                    & T.intercalate " "
                ]


-- LOGGER


data Logger = Logger
    { config :: Config
    , channel :: TChan.TChan Event
    }


instance Show Logger where
    show Logger{..} =
        show config


newLogger :: Config -> IO Logger
newLogger config = do
    channel <- TChan.newTChanIO
    pure Logger{..}


log :: Logger -> Event -> IO ()
log Logger{..} event =
    STM.atomically (TChan.writeTChan channel event)


runLogger :: Logger -> IO a
runLogger (Logger Config{..} channel) =
    Monad.forever $ do
        event <- STM.atomically (TChan.readTChan channel)
        TextIO.putStrLn (formatEvent format event)
