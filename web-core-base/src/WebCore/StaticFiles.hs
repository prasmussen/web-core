{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module WebCore.StaticFiles
    ( Api
    , server
    ) where


import Servant

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static
import qualified WebCore.Server as Server



type Api
      = "static" :> Servant.Raw
    :<|> Servant.Raw


server :: Server.Config -> Servant.Server Api
server config
      =  staticFilesHandler config
    :<|> indexHandler config



staticFilesHandler :: Server.Config -> ServerT Raw m
staticFilesHandler config =
    Servant.serveDirectoryWebApp (Server.staticFilePath config <> "/static")



indexHandler :: Server.Config -> ServerT Raw m
indexHandler config =
    Servant.serveDirectoryWith $
        (Static.defaultWebAppSettings undefined)
            { Static.ssLookupFile = lookupFile (Server.staticFilePath config) "index.html"
            }



lookupFile :: FilePath -> String -> [Static.Piece] -> IO Static.LookupResult
lookupFile basepath name _ = do
    content <- BS.readFile (basepath <> "/" <> name)
    pure $ Static.LRFile $ Static.File
        { fileGetSize = fromIntegral $ BS.length content
        , fileToResponse = \status headers ->
            Wai.responseBuilder status headers (Builder.byteString content)
        , fileName = Static.unsafeToPiece (T.pack name)
        , fileGetHash = pure Nothing
        , fileGetModified = Nothing
        }
