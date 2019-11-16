{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


module WebCore.RateLimit
    ( RateLimitHeaders
    , Info(..)
    , Limit(..)
    , Remaining(..)
    , isAllowed
    , decrementRemaining
    , addHeaders
    ) where


import Control.Lens ((&), (.~))
import Servant

import qualified Data.Swagger as Swagger
import qualified GHC.Generics as GHC
import qualified Web.HttpApiData as HttpApiData



-- SERVANT HEADERS

type RateLimitHeaders =
    '[ Header "X-RateLimit-Limit" Limit
     , Header "X-RateLimit-Remaining" Remaining
     ]



-- INFO

data Info = Info
    { limit :: Limit
    , remaining :: Remaining
    }


isAllowed :: Info -> Bool
isAllowed Info { remaining = Remaining remaining } =
    remaining > 0


decrementRemaining :: Info -> Info
decrementRemaining info@Info { remaining = Remaining remaining } =
    info
        { remaining = Remaining (remaining - 1)
        }


addHeaders
    :: AddHeader h1 Remaining orig1 orig2
    => AddHeader h2 Limit orig2 b
    => Info
    -> orig1
    -> b
addHeaders Info{..} resp =
    resp
        & addHeader remaining
        & addHeader limit


-- LIMIT


newtype Limit = Limit Integer
    deriving (Show, GHC.Generic)
    deriving newtype (HttpApiData.ToHttpApiData)


instance Swagger.ToParamSchema Limit where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerInteger



-- REMAINING


newtype Remaining = Remaining Integer
    deriving (Show, GHC.Generic)
    deriving newtype (HttpApiData.ToHttpApiData)


instance Swagger.ToParamSchema Remaining where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerInteger
