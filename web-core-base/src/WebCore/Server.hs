{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebCore.Server
    ( Config(..)
    , Environment(..)
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
    , baseUrl :: BaseUrl
    , staticPath :: StaticPath
    }
    deriving (Show)


formatConfig :: Config -> T.Text
formatConfig Config
    { listenHost = ListenHost listenHost
    , listenPort = ListenPort listenPort
    , staticPath = StaticPath staticPath
    , baseUrl = BaseUrl baseUrl
    , environment = environment
    } =
    [ ("ListenHost", T.pack $ show listenHost)
    , ("ListenPort", T.pack $ show listenPort)
    , ("Environment", T.pack $ show environment)
    , ("BaseUrl", baseUrl)
    , ("StaticPath", staticPath)
    ]
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


-- TODO: take SystemdConfig
runSystemd :: Config -> Wai.Application -> IO ()
runSystemd config app =
    Systemd.runSystemdWarp systemdSettings (warpSettings config) app


warpSettings :: Config -> Warp.Settings
warpSettings Config
    { listenHost = ListenHost host
    , listenPort = ListenPort port
    } =
    Warp.defaultSettings
        & Warp.setHost host
        & Warp.setPort port


systemdSettings :: Systemd.SystemdSettings
systemdSettings =
    Systemd.defaultSystemdSettings
        & Systemd.setRequireSocketActivation True
        & Systemd.setHeartbeatInterval (Just 5)


-- CONFIG ACCESSOR FUNCTIONS


staticFilePath :: Config -> FilePath
staticFilePath Config{staticPath = StaticPath staticPath} =
    T.unpack staticPath



baseUrlText :: Config -> T.Text
baseUrlText Config{baseUrl = BaseUrl baseUrl} =
    baseUrl
