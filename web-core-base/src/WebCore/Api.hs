{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module WebCore.Api
    ( NotFoundFallbackRoute
    , notFoundFallbackHandler
    ) where


import Servant

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai


type NotFoundFallbackRoute = "api" :> Servant.Raw


notFoundFallbackHandler :: Tagged Handler Application
notFoundFallbackHandler =
    Tagged $ \_ respond ->
        respond $ Wai.responseLBS HTTP.notFound404 [] "Not Found"
