{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Database.PostgreSQL.TransactionalStore
    ( PGTransaction
    , TransactionalStore (..)
    , runPGTransaction
    , query
    , query_
    , execute
    , executeOne
    , executeMany
    , returning
    , queryHead
    ) where

import           Control.Monad.Reader
import           Data.Int
import qualified Database.PostgreSQL.Simple         as Postgres
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow

newtype PGTransaction a =
    PGTransaction (ReaderT Postgres.Connection IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Postgres.Connection
             )

runPGTransaction :: MonadIO m => PGTransaction a -> Postgres.Connection -> m a
runPGTransaction (PGTransaction pgTrans) conn =
    liftIO (Postgres.withTransaction conn (runReaderT pgTrans conn))

-- | Used to execute a `HeliumStore' `m' value inside of a transaction, with
-- connection/state `a', with effects in `n'.
class TransactionalStore a m n where
    runTransaction :: m b -> a -> n b

instance MonadIO m => TransactionalStore Postgres.Connection PGTransaction m where
    runTransaction = runPGTransaction

-- | Issue an SQL query, taking a 'ToRow' input and yielding 'FromRow' outputs.
query :: (ToRow input, FromRow output)
      => Postgres.Query
      -> input
      -> PGTransaction [output]
query q params = ask >>= (\conn -> liftIO $ Postgres.query conn q params)

-- | As 'query', but for queries that take no arguments.
query_ :: (FromRow output) => Postgres.Query -> PGTransaction [output]
query_ q = ask >>= liftIO . (`Postgres.query_` q)

-- | Run a single SQL action and return success.
execute :: ToRow input => Postgres.Query -> input -> PGTransaction Int64
execute q params = ask >>=  (\conn -> liftIO $ Postgres.execute conn q params)

executeMany :: ToRow input => Postgres.Query -> [input] -> PGTransaction Int64
executeMany q params = ask >>=  (\conn -> liftIO $ Postgres.executeMany conn q params)

returning :: (ToRow input, FromRow output)
          => Postgres.Query
          -> [input]
          -> PGTransaction [output]
returning q params = ask >>= (\conn -> liftIO $ Postgres.returning conn q params)

-- | Run a query and return 'Just' the first result found or 'Nothing'.
queryHead :: (ToRow input, FromRow output)
          => input
          -> Postgres.Query
          -> PGTransaction (Maybe output)
queryHead params q = do
  results <- query q params
  return $ case results of
    (a:_) -> Just a
    _     -> Nothing

-- | Run a statement and return 'True' if only a single record was modified.
executeOne :: (ToRow input) => input -> Postgres.Query -> PGTransaction Bool
executeOne params q = do
  results <- execute q params
  return (results == 1)
