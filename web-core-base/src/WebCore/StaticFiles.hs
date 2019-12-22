{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module WebCore.StaticFiles
    ( Api
    , server
    ) where


import Data.Function ((&))
import Servant
import System.FilePath ((</>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
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
    Servant.serveDirectoryWith $
        (Static.defaultWebAppSettings undefined)
            { Static.ssLookupFile = lookupFile (Server.staticFilePath config </> "static")
            , Static.ssMaxAge = Static.NoMaxAge
            }


indexHandler :: Server.Config -> ServerT Raw m
indexHandler config =
    Servant.serveDirectoryWith $
        (Static.defaultWebAppSettings undefined)
            { Static.ssLookupFile = \_ ->
                prepareFile (Server.staticFilePath config </> "index.html")
            , Static.ssMaxAge = Static.NoMaxAge
            }


lookupFile :: FilePath -> [Static.Piece] -> IO Static.LookupResult
lookupFile basePath pieces =
    let
        filePath =
            basePath </> piecesToFilePath pieces
    in do
    fileExists <- Directory.doesFileExist filePath
    if fileExists then do
        prepareFile filePath

    else
        pure Static.LRNotFound


prepareFile :: FilePath -> IO Static.LookupResult
prepareFile filePath = do
    content <- BS.readFile filePath
    pure $ Static.LRFile $ Static.File
        { fileGetSize = fromIntegral (BS.length content)
        , fileToResponse = \status headers ->
            Wai.responseBuilder status headers (Builder.byteString content)
        , fileName =
            filePath
                & FilePath.takeFileName
                & T.pack
                & Static.unsafeToPiece
        , fileGetHash = pure (hashFromFilePath filePath)
        , fileGetModified = Nothing
        }


hashFromFilePath :: FilePath -> Maybe BS.ByteString
hashFromFilePath filePath = do
    nixPath <- BSU.fromString filePath
        & BS.stripPrefix "/nix/store/"
    pure (BS.take 32 nixPath)



piecesToFilePath :: [Static.Piece] -> FilePath
piecesToFilePath pieces =
    pieces
        & map Static.fromPiece
        & filter (not . T.null)
        & T.intercalate "/"
        & T.unpack
