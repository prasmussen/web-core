{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebToolkit.Database
    ( Config(..)
    , ConnectionPool
    , createPool
    , runMigration
    , run
    , runTransaction
    ) where


import Squeal.PostgreSQL

import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Bifoldable as Bifoldable
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock as Clock
import qualified Squeal.PostgreSQL.Pool as Pool
import qualified Squeal.PostgreSQL.PQ as PQ
import qualified Squeal.PostgreSQL.Transaction as Transaction
import qualified UnliftIO
import qualified WebToolkit.Read as Read



-- CONNECTION POOL

newtype ConnectionPool (schemas :: SchemasType) = ConnectionPool (Pool.Pool (K Connection schemas))
    deriving (Show)


createPool :: UnliftIO.MonadUnliftIO m => Config -> m (ConnectionPool schemas)
createPool config@Config
    { maxIdleSeconds = MaxIdleSeconds maxIdleSeconds
    , maxConnections = MaxConnections maxConnections
    } =
    Pool.createConnectionPool (buildConnString config) 1 (fromIntegral maxIdleSeconds) maxConnections
        & fmap ConnectionPool



-- RUN FUNCTIONS

runMigration
    :: Migratory p
    => Config
    -> AlignedList (Migration p) schemas0 schemas1
    -> IO (Either SquealException ())
runMigration config migrations =
    withConnection (buildConnString config) (migrateUp migrations)
        & UnliftIO.try


run
    :: UnliftIO.MonadUnliftIO m
    => ConnectionPool schemas
    -> PQ schemas schemas m x
    -> m (Either SquealException x)
run (ConnectionPool pool) session =
    Pool.usingConnectionPool pool session
        & UnliftIO.try


runTransaction
    :: UnliftIO.MonadUnliftIO m
    => ConnectionPool schemas
    -> (SquealException -> e)
    -> PQ schemas schemas m (Either e x)
    -> m (Either e x)
runTransaction (ConnectionPool pool) mapException session = do
    res <- Pool.usingConnectionPool pool (transactionallyEither defTransactionMode session)
        & UnliftIO.try
    case res of
        Left exception ->
            pure $ Left $ mapException exception

        Right res ->
            pure res


transactionallyEither
    :: (UnliftIO.MonadUnliftIO tx, MonadPQ schemas tx)
    => TransactionMode
    -> tx (Either e x)
    -> tx (Either e x)
transactionallyEither mode tx =
    UnliftIO.mask $ \restore -> do
        manipulate_ $ begin mode
        result <- restore tx `UnliftIO.onException` (manipulate_ rollback)
        case result of
            Left _ ->
                manipulate_ rollback

            Right _ ->
                manipulate_ commit
        return result


defTransactionMode :: TransactionMode
defTransactionMode =
    TransactionMode ReadCommitted ReadWrite NotDeferrable



-- CONFIG

data Config = Config
    { host :: Host
    , name :: Name
    , user :: User
    , pass :: Password
    , port :: Port
    , maxConnections :: MaxConnections
    , maxIdleSeconds :: MaxIdleSeconds
    }
    deriving (Show)

newtype Host = Host T.Text
    deriving (Show)

instance Read Host where
    readsPrec _ = Read.readText Host


newtype Port = Port Int
    deriving (Show)

instance Read Port where
    readsPrec _ = Read.read Port


newtype Name = Name T.Text
    deriving (Show)

instance Read Name where
    readsPrec _ = Read.readText Name


newtype User = User T.Text
    deriving (Show)

instance Read User where
    readsPrec _ = Read.readText User


newtype Password = Password T.Text
    deriving (Show)

instance Read Password where
    readsPrec _ = Read.readText Password


newtype MaxConnections = MaxConnections Int
    deriving (Show)

instance Read MaxConnections where
    readsPrec _ = Read.read MaxConnections


newtype MaxIdleSeconds = MaxIdleSeconds Int
    deriving (Show)

instance Read MaxIdleSeconds where
    readsPrec _ = Read.read MaxIdleSeconds



buildConnString :: Config -> BS.ByteString
buildConnString Config
    { host = Host host
    , name = Name name
    , user = User user
    , pass = Password pass
    , port = Port port
    } =
    [ ("host", host)
    , ("dbname", name)
    , ("user", user)
    , ("password", pass)
    , ("port", T.pack $ show port)
    ]
    & propertyListToString
    & TE.encodeUtf8


propertyListToString :: [(T.Text, T.Text)] -> T.Text
propertyListToString proplist =
    proplist
        & map Bifoldable.biList
        & map (T.intercalate "=")
        & T.intercalate " "
