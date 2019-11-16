{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebCore.Middleware.JsonErrors
    ( jsonErrors
    ) where


import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Status as Status
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai.Internal
import qualified WebCore.Data.Maybe as Maybe


jsonErrors :: Wai.Application -> Wai.Application
jsonErrors =
    Wai.modifyResponse responseModifier


responseModifier :: Wai.Response -> Wai.Response
responseModifier res =
    responseData res
        & Maybe.maybe res jsonErrorResponse


jsonErrorResponse :: ResponseData -> Wai.Response
jsonErrorResponse ResponseData{..} =
    Wai.responseBuilder status (prepareHeaders headers) (prepareBody status body)


prepareHeaders :: Header.ResponseHeaders -> Header.ResponseHeaders
prepareHeaders headers =
    ("Content-Type", "application/json") : headers


prepareBody :: Status.Status -> BS.ByteString -> Builder.Builder
prepareBody status body =
    [ "error" .= Aeson.String (Encoding.decodeUtf8 body)
    , "status" .= Aeson.Number (fromIntegral $ Status.statusCode status)
    ]
    & Aeson.object
    & Aeson.encode
    & Builder.fromLazyByteString


data ResponseData = ResponseData
    { status :: Status.Status
    , headers :: Header.ResponseHeaders
    , body :: BS.ByteString
    }


responseData :: Wai.Response -> Maybe ResponseData
responseData res = do
    status <- Just (Wai.responseStatus res)
    body <- currentBody res
    Status.statusCode status >= 400
        & Maybe.fromBool ()
    pure $ ResponseData
        { status = status
        , headers = Wai.responseHeaders res
        , body =
            if LBS.null body then
                Status.statusMessage status
            else
                LBS.toStrict body
        }


currentBody :: Wai.Response -> Maybe LBS.ByteString
currentBody res =
    case res of
        (Wai.Internal.ResponseBuilder _ _ body) ->
            Just (Builder.toLazyByteString body)

        (Wai.Internal.ResponseRaw _ raw) ->
            currentBody raw

        (Wai.Internal.ResponseFile _ _ _ _) ->
            Nothing

        (Wai.Internal.ResponseStream _ _ _) ->
            Nothing
