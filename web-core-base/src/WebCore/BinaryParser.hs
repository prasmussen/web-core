{-# LANGUAGE OverloadedStrings #-}

module WebCore.BinaryParser
    ( parser
    , maybeParser
    , eitherParser
    ) where


import BinaryParser (BinaryParser)

import qualified BinaryParser
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE



parser :: (BS.ByteString -> a) -> BinaryParser a
parser mapper =
    mapper <$> BinaryParser.remainders


maybeParser :: (BS.ByteString -> Maybe a) -> BinaryParser a
maybeParser mapper = do
    remainders <- BinaryParser.remainders
    case mapper remainders of
        Just res ->
            pure res

        Nothing ->
            BinaryParser.failure $ mconcat
                [ "Failed to parse: «"
                , TE.decodeUtf8 remainders
                , "»"
                ]


eitherParser :: Show e => (BS.ByteString -> Either e a) -> BinaryParser a
eitherParser mapper = do
    remainders <- BinaryParser.remainders
    case mapper remainders of
        Right res ->
            pure res

        Left err ->
            BinaryParser.failure $ mconcat
                [ "Failed to parse: «"
                , TE.decodeUtf8 remainders
                , "», "
                , T.pack (show err)
                ]
