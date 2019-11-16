{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module WebCore.SwaggerUI
    ( Route
    , server
    ) where


import Data.Function ((&))
import Data.Swagger (Swagger)

import qualified Servant
import qualified Servant.Swagger
import qualified Servant.Swagger.UI as SwaggerUI
import qualified Servant.Swagger.UI.Core as SwaggerUICore



type Route =
    SwaggerUI.SwaggerSchemaUI "swagger" "swagger.json"


server
    :: Servant.Swagger.HasSwagger api
    => Servant.ServerT uiApi Servant.Handler ~ Servant.Handler Swagger
    => (Swagger -> Swagger)
    -> Servant.Proxy api
    -> Servant.Server (SwaggerUICore.SwaggerSchemaUI' dir uiApi)
server mapper proxy =
    Servant.Swagger.toSwagger proxy
        & mapper
        & SwaggerUI.swaggerSchemaUIServer
