{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Database.PostgreSQL.Transaction
    ( PGTransaction
    , runPGTransactionT
    , runPGTransactionT'
    , runPGTransactionIO
    , query
    , query_
    , execute
    , executeOne
    , executeMany
    , returning
    , queryHead
    , queryOnly
    , formatQuery
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Int
import qualified Database.PostgreSQL.Simple             as Postgres
import           Database.PostgreSQL.Simple.FromField
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

runPGTransactionT' :: MonadBaseControl IO m
                   => Postgres.Transaction.IsolationLevel
                   -> PGTransactionT m a
                   -> Postgres.Connection
                   -> m a
runPGTransactionT' isolation (PGTransactionT pgTrans) conn =
    let runTransaction run =
          Postgres.Transaction.withTransactionLevel isolation conn (run pgTrans)
    in control runTransaction `runReaderT` conn

runPGTransactionT :: MonadBaseControl IO m
                  => PGTransactionT m a
                  -> Postgres.Connection
                  -> m a
runPGTransactionT = runPGTransactionT' Postgres.Transaction.DefaultIsolationLevel


-- | Convenience function when there are no embedded monadic effects, only IO.
runPGTransactionIO :: MonadIO m
                   => PGTransaction a
                   -> Postgres.Connection
                   -> m a
runPGTransactionIO = (liftIO .) . runPGTransactionT


-- | Issue an SQL query, taking a 'ToRow' input and yielding 'FromRow' outputs.
-- Please note that the parameter order is different from that in the parent
-- postgresql-simple library; this is an intentional choice to improve the aesthetics
-- when using the SQL quasiquoter (making the query parameters come first means that
-- there is more room for the query string).
query :: (ToRow input, FromRow output, MonadIO m)
      => input
      -> Postgres.Query
      -> PGTransactionT m [output]
query params q = ask >>= (\conn -> liftIO $ Postgres.query conn q params)

-- | As 'query', but for queries that take no arguments.
query_ :: (FromRow output, MonadIO m)
       => Postgres.Query
       -> PGTransactionT m [output]
query_ q = ask >>= liftIO . (`Postgres.query_` q)

-- | Run a single SQL action and return success.
execute :: (ToRow input, MonadIO m)
        => input
        -> Postgres.Query
        -> PGTransactionT m Int64
execute params q = ask >>= (\conn -> liftIO $ Postgres.execute conn q params)

executeMany :: (ToRow input, MonadIO m)
            => [input]
            -> Postgres.Query
            -> PGTransactionT m Int64
executeMany params q = ask >>= (\conn -> liftIO $ Postgres.executeMany conn q params)

returning :: (ToRow input, FromRow output, MonadIO m)
          => [input]
          -> Postgres.Query
          -> PGTransactionT m [output]
returning params q = ask >>= (\conn -> liftIO $ Postgres.returning conn q params)

-- | Run a query and return 'Just' the first result found or 'Nothing'.
queryHead :: (ToRow input, FromRow output, MonadIO m)
          => input
          -> Postgres.Query
          -> PGTransactionT m (Maybe output)
queryHead params q = do
  results <- query params q
  return $ case results of
    (a:_) -> Just a
    _     -> Nothing

-- | Run a statement and return 'True' if only a single record was modified.
executeOne :: (ToRow input, MonadIO m)
           => input
           -> Postgres.Query
           -> PGTransactionT m Bool
executeOne params q = (== 1) <$> execute params q

-- | Lookup a single FromField value. This takes care of handling 'Only' for you.
queryOnly :: (ToRow input, FromField f, MonadIO m)
          => input
          -> Postgres.Query
          -> PGTransactionT m (Maybe f)
queryOnly params q = fmap Postgres.fromOnly <$> queryHead params q

formatQuery :: (ToRow input, MonadIO m)
            => input
            -> Postgres.Query
            -> PGTransactionT m Postgres.Query
formatQuery params q = do
    conn <- ask
    liftIO (PGTypes.Query <$> Postgres.formatQuery conn q params)
