{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebCore.Cookie
    ( Config(..)
    , CookieJar
    , SetCookie
    , setCookie
    , setExpiredCookie
    , setCookiePath
    , lookup
    ) where


import Data.Function ((&))
import Prelude hiding (lookup)

import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock as Clock
import qualified GHC.Generics as GHC
import qualified Prelude as Prelude
import qualified Web.Cookie as WebCookie
import qualified Web.HttpApiData as HttpApiData
import qualified WebCore.Read as Read



-- CONFIG

data Config = Config
    { maxAge :: MaxAge
    , httpOnly :: HttpOnly
    , secure :: Secure
    , sameSite :: SameSite
    }
    deriving (Show)


newtype MaxAge = MaxAge Clock.DiffTime
    deriving (Show)


instance Read MaxAge where
    readsPrec _ str =
        Read.read (MaxAge . Clock.secondsToDiffTime) str


newtype HttpOnly = HttpOnly Bool
    deriving (Show)


instance Read HttpOnly where
    readsPrec _ str =
        Read.readBool HttpOnly str


newtype Secure = Secure Bool
    deriving (Show)


instance Read Secure where
    readsPrec _ str =
        Read.readBool Secure str



newtype SameSite = SameSite WebCookie.SameSiteOption
    deriving (Show)


instance Read SameSite where
    readsPrec _ str =
        case str of
            "lax" ->
                [(SameSite WebCookie.sameSiteLax, "")]

            "strict" ->
                [(SameSite WebCookie.sameSiteStrict, "")]

            _ ->
                []


-- COOKIE


newtype CookieJar = CookieJar WebCookie.Cookies
    deriving (Show, Eq, Ord, GHC.Generic)


instance HttpApiData.FromHttpApiData CookieJar where
    parseUrlPiece text =
        text
            & TE.encodeUtf8
            & WebCookie.parseCookies
            & CookieJar
            & Right

    parseHeader header =
        header
            & WebCookie.parseCookies
            & CookieJar
            & Right


newtype SetCookie = SetCookie WebCookie.SetCookie
    deriving (Show, GHC.Generic)


instance HttpApiData.ToHttpApiData SetCookie where
    toUrlPiece (SetCookie cookie) =
        cookie
            & WebCookie.renderSetCookie
            & Builder.toLazyByteString
            & LBS.toStrict
            & TE.decodeUtf8

    toHeader (SetCookie cookie) =
        cookie
            & WebCookie.renderSetCookie
            & Builder.toLazyByteString
            & LBS.toStrict


lookup :: CookieJar -> BS.ByteString -> Maybe BS.ByteString
lookup (CookieJar cookieJar) name =
    Prelude.lookup name cookieJar


setCookie :: Config -> BS.ByteString -> BS.ByteString -> SetCookie
setCookie config name value =
    setCookieFromConfig config
        & setName name
        & setValue value


setExpiredCookie :: Config -> BS.ByteString -> SetCookie
setExpiredCookie config name =
    setCookie config name ""
        & setMaxAge (Just 0)


setCookieFromConfig :: Config -> SetCookie
setCookieFromConfig Config
    { maxAge = MaxAge maxAge
    , httpOnly = HttpOnly httpOnly
    , secure = Secure secure
    , sameSite = SameSite sameSite
    } =
    SetCookie $ WebCookie.defaultSetCookie
        { WebCookie.setCookieDomain = Nothing
        , WebCookie.setCookieSameSite = Just sameSite
        , WebCookie.setCookieMaxAge = Just maxAge
        , WebCookie.setCookieHttpOnly = httpOnly
        , WebCookie.setCookieSecure = secure
        , WebCookie.setCookiePath = Just "/"
        }



-- CONFIG FUNCTIONS


setName :: BS.ByteString -> SetCookie -> SetCookie
setName name (SetCookie cookie) =
    SetCookie $ cookie
        { WebCookie.setCookieName = name
        }


setValue :: BS.ByteString -> SetCookie -> SetCookie
setValue value (SetCookie cookie) =
    SetCookie $ cookie
        { WebCookie.setCookieValue = value
        }

setMaxAge :: Maybe Clock.DiffTime -> SetCookie -> SetCookie
setMaxAge maybeMaxAge (SetCookie cookie) =
    SetCookie $ cookie
        { WebCookie.setCookieMaxAge = maybeMaxAge
        }


setCookiePath :: Maybe BS.ByteString -> SetCookie -> SetCookie
setCookiePath maybePath (SetCookie cookie) =
    SetCookie $ cookie
        { WebCookie.setCookiePath = maybePath
        }
