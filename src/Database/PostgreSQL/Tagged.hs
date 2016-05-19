{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitPrelude            #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-|
Module      : Database.PostgreSQL.Tagged
Copyright   : (c) Helium Systems, Inc.
License     : MIT
Maintainer  : patrick@helium.com
Stability   : experimental
Portability : GHC

This module is similar to @Database.PostgreSQL.Simple@, with one notable exception:
each function is tagged (using DataKinds over the 'Effect' type) with information
as to whether it reads from or writes to the database. This is useful in conjunction
with Postgres setups that are replicated over multiple machines.

As with 'Database.PostgreSQL.Transaction', the parameter order is reversed when compared to the functions
provided by postgresql-simple.

-}

module Database.PostgreSQL.Tagged
    ( Effect (..)
    , PGTaggedT
    , PGTaggedIO
    , whileWriting
    , runPGWrite
    , runPGRead
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
import           Data.Coerce
import           Data.Int
import qualified Database.PostgreSQL.Simple           as Postgres
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import qualified Database.PostgreSQL.Transaction      as T

data Effect = Read | Write

-- | The tagged-effect Postgres monad transformer.
newtype PGTaggedT (e :: Effect) m a =
    PGTagged (T.PGTransactionT m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadTrans
             , MonadReader Postgres.Connection
             , MonadIO)

type PGTaggedIO e a = PGTaggedT e IO a

-- | Run a writing-oriented PGTagged transaction.
runPGWrite :: MonadBaseControl IO m
             => PGTaggedT 'Write m a
             -> Postgres.Connection
             -> m a
runPGWrite = T.runPGTransactionT . coerce

-- | Run a read-only PGTagged transaction. Actions such as
-- these can take place on a read-only replica.
runPGRead :: MonadBaseControl IO m
             => PGTaggedT 'Read m a
             -> Postgres.Connection
             -> m a
runPGRead = T.runPGTransactionT . coerce

-- | Promote a reading operation to a writing operation.
-- Note that there is no way to go the opposite direction
-- (unless you use 'coerce'). This is by design: if you're
-- writing, it's safe to read, but the converse does not
-- necessarily hold true.
whileWriting :: PGTaggedT 'Read m a -> PGTaggedT 'Write m a
whileWriting = coerce

-- | Run an individual query. (read operation)
query :: (ToRow input, FromRow output, MonadIO m)
      => input
      -> Postgres.Query
      -> PGTaggedT 'Read m [output]
query i = PGTagged <$> T.query i

-- | As 'query', but without arguments. (read operation)
query_ :: (FromRow output, MonadIO m)
       => Postgres.Query
       -> PGTaggedT 'Read m [output]
query_ = PGTagged <$> T.query_

-- | As 'Database.PostgreSQL.Simple.execute'. (write operation)
execute :: (ToRow input, MonadIO m)
        => input
        -> Postgres.Query
        -> PGTaggedT 'Write m Int64
execute i = PGTagged <$> T.execute i

-- | As 'Database.PostgreSQL.Simple.executeMany'. (write operation)
executeMany :: (ToRow input, MonadIO m)
            => [input]
            -> Postgres.Query
            -> PGTaggedT 'Write m Int64
executeMany is = PGTagged <$> T.executeMany is

-- | As 'Database.PostgreSQL.Simple.returning'. (write operation)
returning :: (ToRow input, FromRow output, MonadIO m)
          => [input]
          -> Postgres.Query
          -> PGTaggedT 'Write m [output]
returning is = PGTagged <$> T.returning is

-- | As 'Database.PostgreSQL.Transaction.queryOnly'. (read operation)
queryOnly :: (ToRow input, FromField f, MonadIO m)
          => input
          -> Postgres.Query
          -> PGTaggedT 'Read m (Maybe f)
queryOnly i = PGTagged <$> T.queryOnly i

-- | As 'Database.PostgreSQL.Transaction.queryHead'. (read operation)
queryHead :: (ToRow input, FromRow output, MonadIO m)
          => input
          -> Postgres.Query
          -> PGTaggedT 'Read m (Maybe output)
queryHead i = PGTagged <$> T.queryHead i

-- | As 'Database.PostgreSQL.Transaction.executeOne'. (write operation)
executeOne :: (ToRow input, MonadIO m)
           => input
           -> Postgres.Query
           -> PGTaggedT 'Write m Bool
executeOne i = PGTagged <$> T.executeOne i

-- | As 'Database.PostgreSQL.Simple.formatQuery'. (neutral)
formatQuery :: (ToRow input, MonadIO m)
            => input
            -> Postgres.Query
            -> PGTaggedT e m Postgres.Query
formatQuery i = PGTagged <$> T.formatQuery i
