module WebToolkit.Crypto
    ( EncryptionKey
    , Ciphertext(..)
    , newKey
    , keyToText
    , encrypt
    , decrypt
    , toCiphertext
    ) where


import Data.Function ((&))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Either.Combinators as Combinators
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Web.ClientSession as CS



newtype EncryptionKey = EncryptionKey (BS.ByteString, CS.Key)
    deriving (Show)


instance Read EncryptionKey where
    readsPrec _ str =
        case keyFromText (T.pack str) of
            Just key ->
                [(key, "")]

            Nothing ->
                []


newKey :: IO EncryptionKey
newKey = do
    keyTuple <- CS.randomKey
    pure (EncryptionKey keyTuple)


keyToText :: EncryptionKey -> T.Text
keyToText (EncryptionKey (bs, _)) =
    TE.decodeUtf8 (Base64.encode bs)


keyFromText :: T.Text -> Maybe EncryptionKey
keyFromText base64 = do
    bs <- Base64.decode (TE.encodeUtf8 base64)
        & Combinators.rightToMaybe
    key <- CS.initKey bs
        & Combinators.rightToMaybe
    pure $ EncryptionKey (bs, key)



-- CIPHERTEXT

newtype Ciphertext = Ciphertext BS.ByteString
    deriving (Show)


toCiphertext :: BS.ByteString -> Ciphertext
toCiphertext bs =
    Ciphertext bs


encrypt :: EncryptionKey -> BS.ByteString -> IO Ciphertext
encrypt (EncryptionKey (_, key)) plaintext =
    Ciphertext <$> CS.encryptIO key plaintext


decrypt :: EncryptionKey -> Ciphertext -> Maybe BS.ByteString
decrypt (EncryptionKey (_, key)) (Ciphertext ciphertext) =
    CS.decrypt key ciphertext
