{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Database.PostgreSQL.TransactionalStore
    ( PGTransaction
    , runPGTransaction
    , runPGTransaction'
    , runPGTransactionIO
    , query
    , query_
    , execute
    , executeOne
    , executeMany
    , returning
    , queryHead
    , formatQuery
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Int
import qualified Database.PostgreSQL.Simple             as Postgres
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import qualified Database.PostgreSQL.Simple.Transaction as Postgres.Transaction
import qualified Database.PostgreSQL.Simple.Types       as PGTypes

newtype PGTransactionT m a =
    PGTransactionT (ReaderT Postgres.Connection m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadTrans
             , MonadReader Postgres.Connection
             , MonadIO
             )

type PGTransaction = PGTransactionT IO

runPGTransaction' :: MonadBaseControl IO m
                  => Postgres.Transaction.IsolationLevel
                  -> PGTransactionT m a
                  -> Postgres.Connection -> m a
runPGTransaction' isolation (PGTransactionT pgTrans) conn =
    let runTransaction run =
          Postgres.Transaction.withTransactionLevel isolation conn (run pgTrans)
    in control runTransaction `runReaderT` conn

runPGTransaction :: MonadBaseControl IO m
                 => PGTransactionT m a
                 -> Postgres.Connection
                 -> m a
runPGTransaction = runPGTransaction' Postgres.Transaction.DefaultIsolationLevel


-- | Convenience function when there are no embedded monadic effects, only IO.
runPGTransactionIO :: MonadIO m
                   => PGTransaction a
                   -> Postgres.Connection
                   -> m a
runPGTransactionIO = (liftIO .) . runPGTransaction


-- | Issue an SQL query, taking a 'ToRow' input and yielding 'FromRow' outputs.
query :: (ToRow input, FromRow output, MonadIO m)
      => Postgres.Query
      -> input
      -> PGTransactionT m [output]
query q params = ask >>= (\conn -> liftIO $ Postgres.query conn q params)

-- | As 'query', but for queries that take no arguments.
query_ :: (FromRow output, MonadIO m)
       => Postgres.Query
       -> PGTransactionT m [output]
query_ q = ask >>= liftIO . (`Postgres.query_` q)

-- | Run a single SQL action and return success.
execute :: (ToRow input, MonadIO m)
        => Postgres.Query
        -> input
        -> PGTransactionT m Int64
execute q params = ask >>= (\conn -> liftIO $ Postgres.execute conn q params)

executeMany :: (ToRow input, MonadIO m)
            => Postgres.Query
            -> [input]
            -> PGTransactionT m Int64
executeMany q params = ask >>= (\conn -> liftIO $ Postgres.executeMany conn q params)

returning :: (ToRow input, FromRow output, MonadIO m)
          => Postgres.Query
          -> [input]
          -> PGTransactionT m [output]
returning q params = ask >>= (\conn -> liftIO $ Postgres.returning conn q params)

-- | Run a query and return 'Just' the first result found or 'Nothing'.
queryHead :: (ToRow input, FromRow output, MonadIO m)
          => input
          -> Postgres.Query
          -> PGTransactionT m (Maybe output)
queryHead params q = do
  results <- query q params
  return $ case results of
    (a:_) -> Just a
    _     -> Nothing

-- | Run a statement and return 'True' if only a single record was modified.
executeOne :: (ToRow input, MonadIO m)
           => input
           -> Postgres.Query
           -> PGTransactionT m Bool
executeOne params q = do
  results <- execute q params
  return (results == 1)

formatQuery :: (ToRow q, MonadIO m)
            => Postgres.Query
            -> q
            -> PGTransactionT m Postgres.Query
formatQuery q params = do
    conn <- ask
    liftIO (PGTypes.Query <$> Postgres.formatQuery conn q params)
