{-# LANGUAGE OverloadedStrings #-}

module WebCore.Middleware.NoCache
    ( noCache
    ) where

import Data.Function ((&))
import Network.HTTP.Types.Header (Header)

import qualified Network.HTTP.Types.Header as Header
import qualified Network.Wai as Wai



noCache :: Wai.Application -> Wai.Application
noCache app req respond = do
    app req $ \res -> do
        Wai.mapResponseHeaders updateHeaders res
            & respond


updateHeaders :: [Header] -> [Header]
updateHeaders headers =
    headers
        & filter (not . isCacheHeader)
        & (noCacheHeaders ++)


isCacheHeader :: Header -> Bool
isCacheHeader (name, _) =
    any (== name) cacheHeaderNames


cacheHeaderNames :: [Header.HeaderName]
cacheHeaderNames =
    [ "cache-control"
    , "etag"
    , "expires"
    , "last-modified"
    , "pragma"
    ]


noCacheHeaders :: [Header]
noCacheHeaders =
    [ ("Cache-Control", "no-cache")
    , ("Pragma", "no-cache")
    ]
