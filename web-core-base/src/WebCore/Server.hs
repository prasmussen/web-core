{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebCore.Server
    ( Config(..)
    , SystemdConfig(..)
    , SystemdHooks(..)
    , Environment(..)
    , ListenType(..)
    , run
    , runSystemd
    , baseUrlText
    , staticFilePath
    , formatConfig
    , environmentToText
    ) where


import Data.Function ((&))

import qualified Data.Bifoldable as Bifoldable
import qualified Data.String as String
import qualified Data.Text as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Systemd as Systemd
import qualified WebCore.Read as Read



-- CONFIG


data Config = Config
    { environment :: Environment
    , listenPort :: ListenPort
    , listenHost :: ListenHost
    , listenType :: ListenType
    , baseUrl :: BaseUrl
    , staticPath :: StaticPath
    }
    deriving (Show)


formatConfig :: Config -> T.Text
formatConfig Config
    { listenHost = ListenHost listenHost
    , listenPort = ListenPort listenPort
    , listenType = listenType
    , staticPath = StaticPath staticPath
    , baseUrl = BaseUrl baseUrl
    , environment = environment
    } =
    let
        options =
            case listenType of
                ListenTcp ->
                    [ ("ListenHost", T.pack $ show listenHost)
                    , ("ListenPort", T.pack $ show listenPort)
                    , ("ListenType", T.pack $ show listenType)
                    , ("Environment", T.pack $ show environment)
                    , ("BaseUrl", baseUrl)
                    , ("StaticPath", staticPath)
                    ]

                ListenSocketActivation ->
                    [ ("ListenType", T.pack $ show listenType)
                    , ("Environment", T.pack $ show environment)
                    , ("BaseUrl", baseUrl)
                    , ("StaticPath", staticPath)
                    ]
    in
    options
        & map Bifoldable.biList
        & map (T.intercalate ": ")
        & T.intercalate "\n"


newtype ListenPort = ListenPort Int
    deriving (Show)

instance Read ListenPort where
    readsPrec _ str =
        Read.read ListenPort str


newtype ListenHost = ListenHost Warp.HostPreference
    deriving (Show)

instance Read ListenHost where
    readsPrec _ str =
        Read.readString (ListenHost . String.fromString) str


newtype BaseUrl = BaseUrl T.Text
    deriving (Show)

instance Read BaseUrl where
    readsPrec _ str =
        Read.readText BaseUrl str


newtype StaticPath = StaticPath T.Text
    deriving (Show)

instance Read StaticPath where
    readsPrec _ str =
        Read.readText StaticPath str


-- LISTEN TYPE


data ListenType
    = ListenTcp
    | ListenSocketActivation
    deriving (Show)


instance Read ListenType where
    readsPrec _ str =
        case str of
            "tcp" ->
                [(ListenTcp, "")]

            "socket-activation" ->
                [(ListenSocketActivation, "")]

            _ ->
                []


-- SYSTEMD CONFIG


data SystemdConfig = SystemdConfig
    { enableSocketActivation :: EnableSocketActivation
    , heartbeatInterval :: Maybe HeartbeatInterval
    }
    deriving (Show)


data SystemdHooks = SystemdHooks
    { logWarning :: String -> IO ()
    , logInfo :: String -> IO ()
    , onShutdown :: IO ()
    }


newtype EnableSocketActivation = EnableSocketActivation Bool
    deriving (Show)


instance Read EnableSocketActivation where
    readsPrec _ str =
        Read.readBool EnableSocketActivation str


newtype HeartbeatInterval = HeartbeatInterval Int
    deriving (Show)


instance Read HeartbeatInterval where
    readsPrec _ str =
        Read.read HeartbeatInterval str



heartbeatIntervalInt :: HeartbeatInterval -> Int
heartbeatIntervalInt (HeartbeatInterval n) = n



-- ENVIRONMENT


data Environment
    = Production
    | Staging
    | Development
    deriving (Show)


instance Read Environment where
    readsPrec _ str =
        case str of
            "production" ->
                [(Production, "")]

            "staging" ->
                [(Staging, "")]

            "development" ->
                [(Development, "")]

            _ ->
                []


environmentToText :: Environment -> T.Text
environmentToText environment =
    case environment of
        Production ->
            "production"

        Staging ->
            "staging"

        Development ->
            "development"


-- RUN SERVER


run :: Config -> Wai.Application -> IO ()
run config app =
    Warp.runSettings (warpSettings config) app


runSystemd :: Config -> SystemdConfig -> SystemdHooks -> Wai.Application -> IO ()
runSystemd config systemdConfig hooks app =
    case listenType config of
        ListenTcp ->
            run config app

        ListenSocketActivation ->
            Systemd.runSystemdWarp (systemdSettings systemdConfig hooks) (warpSettings config) app


warpSettings :: Config -> Warp.Settings
warpSettings Config
    { listenHost = ListenHost host
    , listenPort = ListenPort port
    } =
    Warp.defaultSettings
        & Warp.setHost host
        & Warp.setPort port


systemdSettings :: SystemdConfig -> SystemdHooks -> Systemd.SystemdSettings
systemdSettings SystemdConfig
    { enableSocketActivation = EnableSocketActivation enableSocketActivation
    , ..
    } SystemdHooks{..} =
    Systemd.defaultSystemdSettings
        & Systemd.setRequireSocketActivation enableSocketActivation
        & Systemd.setHeartbeatInterval (heartbeatIntervalInt <$> heartbeatInterval)
        & Systemd.setLogWarn logWarning
        & Systemd.setLogInfo logInfo
        & Systemd.setOnBeginShutdown onShutdown


-- CONFIG ACCESSOR FUNCTIONS


staticFilePath :: Config -> FilePath
staticFilePath Config{staticPath = StaticPath staticPath} =
    T.unpack staticPath



baseUrlText :: Config -> T.Text
baseUrlText Config{baseUrl = BaseUrl baseUrl} =
    baseUrl
