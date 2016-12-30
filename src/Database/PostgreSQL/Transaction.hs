{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitPrelude            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|
Module      : Database.PostgreSQL.Transaction
Copyright   : (c) Helium Systems, Inc.
License     : MIT
Maintainer  : patrick@helium.com
Stability   : experimental
Portability : GHC

This module provdes querying with and executing SQL statements that replace
the ones found in @Database.PostgreSQL.Simple@.

Please note that the parameter order is reversed when compared to the functions
provided by postgresql-simple. This is a conscious choice made so as to ease
use of a SQL quasiquoter.

-}

module Database.PostgreSQL.Transaction
    ( PGTransactionT
    , PGTransaction
    , runPGTransactionT
    , runPGTransactionT'
    , runPGTransactionIO
    , mapPGTransactionT
    , query
    , query_
    , execute
    , execute_
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
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Int
import qualified Database.PostgreSQL.Simple             as Postgres
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import qualified Database.PostgreSQL.Simple.Transaction as Postgres.Transaction
import qualified Database.PostgreSQL.Simple.Types       as PGTypes

-- | The Postgres transaction monad transformer. This is implemented as a monad transformer
-- so as to integrate properly with monadic logging libraries like @monad-logger@ or @katip@.
newtype PGTransactionT m a =
    PGTransactionT (ReaderT Postgres.Connection m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadTrans
             , MonadIO
             , MonadThrow
             )

instance MonadReader r m => MonadReader r (PGTransactionT m) where
    ask = lift ask
    local = mapPGTransactionT . local

instance MonadState s m => MonadState s (PGTransactionT m) where
    get = lift get
    put = lift . put

getConnection :: Monad m => PGTransactionT m Postgres.Connection
getConnection = PGTransactionT ask


-- | Transform the computation under PGTransactionT
mapPGTransactionT :: (m a -> n b) -> PGTransactionT m a -> PGTransactionT n b
mapPGTransactionT f (PGTransactionT m) = PGTransactionT $ mapReaderT f m


-- | A type alias for occurrences of 'PGTransactionT' in the IO monad.
type PGTransaction = PGTransactionT IO


-- | Runs a transaction in the base monad @m@ with a provided 'IsolationLevel'.
 -- An instance of MonadBaseControl is required so as to handle lifted calls to 'catch' correctly.
runPGTransactionT' :: MonadBaseControl IO m
                   => Postgres.Transaction.IsolationLevel
                   -> PGTransactionT m a
                   -> Postgres.Connection
                   -> m a
runPGTransactionT' isolation (PGTransactionT pgTrans) conn =
    let runTransaction run =
          Postgres.Transaction.withTransactionLevel isolation conn (run pgTrans)
    in control runTransaction `runReaderT` conn

-- | As 'runPGTransactionT'', but with the 'DefaultIsolationLevel' isolation level.
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
query params q = getConnection >>= (\conn -> liftIO $ Postgres.query conn q params)

-- | As 'query', but for queries that take no arguments.
query_ :: (FromRow output, MonadIO m)
       => Postgres.Query
       -> PGTransactionT m [output]
query_ q = getConnection >>= liftIO . (`Postgres.query_` q)

-- | Run a single SQL action and return success.
execute :: (ToRow input, MonadIO m)
        => input
        -> Postgres.Query
        -> PGTransactionT m Int64
execute params q = getConnection >>= (\conn -> liftIO $ Postgres.execute conn q params)

-- | As 'execute', but for queries that take no arguments.
execute_ :: MonadIO m
         => Postgres.Query
         -> PGTransactionT m Int64
execute_ q = getConnection >>= (\conn -> liftIO $ Postgres.execute_ conn q)

-- | As 'Database.PostgreSQL.Simple.executeMany', but operating in the transaction monad.
-- If any one of these computations fails, the entire block will be rolled back.
executeMany :: (ToRow input, MonadIO m)
            => [input]
            -> Postgres.Query
            -> PGTransactionT m Int64
executeMany params q = getConnection >>= (\conn -> liftIO $ Postgres.executeMany conn q params)

-- | Identical to 'Database.PostgreSQL.Simple.returning', save parameter order.
returning :: (ToRow input, FromRow output, MonadIO m)
          => [input]
          -> Postgres.Query
          -> PGTransactionT m [output]
returning params q = getConnection >>= (\conn -> liftIO $ Postgres.returning conn q params)

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

-- | As 'Database.PostgreSQL.Simple.formatQuery', save parameter order.
formatQuery :: (ToRow input, MonadIO m)
            => input
            -> Postgres.Query
            -> PGTransactionT m Postgres.Query
formatQuery params q = do
    conn <- getConnection
    liftIO (PGTypes.Query <$> Postgres.formatQuery conn q params)
