{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebCore.Server
    ( Config(..)
    , Environment(..)
    , run
    , baseUrlText
    , staticFilePath
    , formatConfig
    ) where


import Data.Function ((&))

import qualified Data.Bifoldable as Bifoldable
import qualified Data.String as String
import qualified Data.Text as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
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
    | Development
    deriving (Show)


instance Read Environment where
    readsPrec _ str =
        case str of
            "production" ->
                [(Production, "")]

            "development" ->
                [(Development, "")]

            _ ->
                []



-- RUN SERVER


run :: Config -> Wai.Application -> IO ()
run Config
    { listenHost = ListenHost host
    , listenPort = ListenPort port
    } app =
    Warp.defaultSettings
        & Warp.setHost host
        & Warp.setPort port
        & \settings -> Warp.runSettings settings app



-- CONFIG ACCESSOR FUNCTIONS


staticFilePath :: Config -> FilePath
staticFilePath Config{staticPath = StaticPath staticPath} =
    T.unpack staticPath



baseUrlText :: Config -> T.Text
baseUrlText Config{baseUrl = BaseUrl baseUrl} =
    baseUrl
