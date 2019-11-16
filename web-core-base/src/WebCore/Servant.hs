{-# LANGUAGE OverloadedStrings #-}

module WebCore.Servant
    ( err429
    , module Servant
    ) where


import Servant


err429 :: ServantErr
err429 =
    ServantErr
        { errHTTPCode = 429
        , errReasonPhrase = "Too Many Requests"
        , errBody = ""
        , errHeaders = []
        }
