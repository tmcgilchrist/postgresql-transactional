# postgresql-transactional

## Summary

`postgresql-transactional` is a simple monadic wrapper around the SQL
primitives introduced by the [postgresql-simple][psqls] package. It provides
simple and predictable semantics for database operations, enforces awareness of
Postgres's transactional nature at API boundaries, and obviates the need for
transaction boilerplate in SQL queries.

## Details

Though the primitives provided by the [postgresql-simple][psqls] package are
fast and powerful, their interface is (by design) very basic: specifically, all
query functions take a shared `Connection` parameter and operate in the `IO`
monad. 

```haskell
query :: FromRow r => Connection -> Query -> IO [r]
execute :: ToRow q => Connection -> Query -> q -> IO Int64
```

By virtue of the fact that (usually) all queries in a given scope are routed
through a single `Connection`, we can abstract away the shared `Connection`
parameter by wrapping a `ReaderT Connection` in a monad transformer:

```haskell
newtype PGTransactionT m a =
    PGTransactionT (ReaderT Postgres.Connection m a)
        deriving (Functor, Applicative, Monad, MonadTrans, MonadIO,
                  MonadReader Postgres.Connection)

type PGTransaction a = PGTransactionT IO a
```

In the common case, the `m` parameter will simply be `IO`. The library provides
the type alias `type PGTransaction a = PGTransactionT IO a` to simplify type
signatures in these cases.

We can then reimplement our query functions in a more natural fashion:

```haskell
query :: (FromRow r, MonadIO m) => Query -> PGTransactionT m [a]
execute :: (ToRow q, MonadIO m) => Query -> q -> PGTransactionT m Int64
```

And we can then use the [postgresql-simple][psqls] `withTransaction` function
to provide `runPGTransaction`, which executes a given `PGTransactionT` block
with rollback semantics:

```haskell
runPGTransaction :: MonadBaseControl IO m => PGTransactionT m a -> Postgres.Connection -> m a
```

Use of the `MonadBaseControl IO m` constraint leaves open the option of
embedding additional effects with the `m` parameter, such as logging, state, or
error-handling.

We also provide a `PGTagged` monad transformer that is equivalent to `PGTransaction`, but includes 
a phantom type in each relevant type signature that indicates whether said function has read-only 
or write-enabled effects. This can be useful when dispatching read-only queries to Postgres replicas.

##  Helium Documentation and Community Support 


* **Docs** Complete documenation for all parts of Helium can be found at [docs.helium.com](https://docs/helium.com). 

* **chat.helium.com** - If you have questions or ideas about how to use this code - or any part of Helium - head over the [chat.helium.com](https://chat.helium.com). We're standing by to help. 


## About

`postgresql-transactional` was extracted from a production Haskell project at
[Helium][helium]. It is open-source software &copy; Helium Systems, Inc., and
released to the public under the terms of the MIT license.

[psqls]: https://github.com/lpsmith/postgresql-simple
[helium]: https://www.helium.com
