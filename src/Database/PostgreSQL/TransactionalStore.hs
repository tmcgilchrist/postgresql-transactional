{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ConstraintKinds            #-}

module Database.PostgreSQL.TransactionalStore
    ( MonadPGTransact
    , PGTransaction
    , TransactionalStore (..)
    , runPGTransaction
    , runPGTransaction'
    , query
    , query_
    , execute
    , executeOne
    , executeMany
    , returning
    , queryHead
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad.Reader
import           Data.Int
import qualified Database.PostgreSQL.Simple         as Postgres
import qualified Database.PostgreSQL.Simple.Transaction as Postgres.Transaction
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow


type MonadPGTransact m = (MonadIO m, MonadReader Postgres.Connection m)


newtype PGTransaction a =
    PGTransaction (ReaderT Postgres.Connection IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Postgres.Connection
             )

runPGTransaction' :: MonadIO m
                  => Postgres.Transaction.IsolationLevel
                  -> PGTransaction a
                  -> Postgres.Connection -> m a
runPGTransaction' isolation (PGTransaction pgTrans) conn =
    liftIO (Postgres.Transaction.withTransactionLevel isolation  conn (runReaderT pgTrans conn))

runPGTransaction :: MonadIO m => PGTransaction a -> Postgres.Connection -> m a
runPGTransaction = runPGTransaction' Postgres.Transaction.DefaultIsolationLevel

-- | Used to execute a `HeliumStore' `m' value inside of a transaction, with
-- connection/state `a', with effects in `n'.
class TransactionalStore a m n where
    runTransaction :: m b -> a -> n b

instance MonadIO m => TransactionalStore Postgres.Connection PGTransaction m where
    runTransaction = runPGTransaction

-- | Issue an SQL query, taking a 'ToRow' input and yielding 'FromRow' outputs.
query :: (ToRow input, FromRow output, MonadPGTransact m)
      => Postgres.Query
      -> input
      -> m [output]
query q params = ask >>= (\conn -> liftIO $ Postgres.query conn q params)

-- | As 'query', but for queries that take no arguments.
query_ :: (FromRow output, MonadPGTransact m)
       => Postgres.Query -> m [output]
query_ q = ask >>= liftIO . (`Postgres.query_` q)

-- | Run a single SQL action and return success.
execute :: (ToRow input, MonadPGTransact m)
        => Postgres.Query -> input -> m Int64
execute q params = ask >>=  (\conn -> liftIO $ Postgres.execute conn q params)

executeMany :: (ToRow input, MonadPGTransact m)
            => Postgres.Query -> [input] -> m Int64
executeMany q params = ask >>=  (\conn -> liftIO $ Postgres.executeMany conn q params)

returning :: (ToRow input, FromRow output, MonadPGTransact m)
          => Postgres.Query
          -> [input]
          -> m [output]
returning q params = ask >>= (\conn -> liftIO $ Postgres.returning conn q params)

-- | Run a query and return 'Just' the first result found or 'Nothing'.
queryHead :: (ToRow input, FromRow output, MonadPGTransact m)
          => input
          -> Postgres.Query
          -> m (Maybe output)
queryHead params q = do
  results <- query q params
  return $ case results of
    (a:_) -> Just a
    _     -> Nothing

-- | Run a statement and return 'True' if only a single record was modified.
executeOne :: (ToRow input, MonadPGTransact m)
           => input -> Postgres.Query -> m Bool
executeOne params q = do
  results <- execute q params
  return (results == 1)
