{-# LANGUAGE OverloadedStrings #-}

module WebCore.Middleware.NoCache
    ( removeAll
    , onlyEtag
    ) where

import Data.Function ((&))
import Network.HTTP.Types.Header (Header)

import qualified Network.HTTP.Types.Header as Header
import qualified Network.Wai as Wai


onlyEtag :: Wai.Application -> Wai.Application
onlyEtag =
    removeHeaders
        [ "cache-control"
        , "pragma"
        , "expires"
        , "last-modified"
        ]


removeAll :: Wai.Application -> Wai.Application
removeAll =
    removeHeaders
        [ "cache-control"
        , "pragma"
        , "expires"
        , "last-modified"
        , "etag"
        ]


removeHeaders :: [Header.HeaderName] -> Wai.Application -> Wai.Application
removeHeaders unwantedHeaders app req respond =
    app req $ \res -> do
        Wai.mapResponseHeaders (updateHeaders unwantedHeaders) res
            & respond


updateHeaders :: [Header.HeaderName] -> [Header] -> [Header]
updateHeaders unwantedHeaders headers =
    headers
        & filter (not . (isUnwantedHeader unwantedHeaders))
        & (noCacheHeaders ++)


isUnwantedHeader :: [Header.HeaderName] -> Header -> Bool
isUnwantedHeader unwantedHeaders (name, _) =
    any (== name) unwantedHeaders


noCacheHeaders :: [Header]
noCacheHeaders =
    [ ("Cache-Control", "no-cache")
    , ("Pragma", "no-cache")
    ]
