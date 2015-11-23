# postgresql-transactional

## Summary

`postgresql-transactional` is a simple monadic wrapper around the SQL primitives introduced by the [postgresql-simple][psqls] package. It provides simple and predictable semantics for database operations, enforces awareness of Postgres's transactional nature at API boundaries, and obviates the need for transaction boilerplate in SQL queries.

## Details

Though the primitives provided by the [postgresql-simple][psqls] package are fast and powerful, their interface is (by design) very basic: specifically, all query functions take a shared `Connection` parameter and operate in the `IO` monad. 

```haskell
query :: FromRow r => Connection -> Query -> IO [r]
execute :: ToRow q => Connection -> Query -> q -> IO Int64
```

By virtue of the fact that (usually) all queries in a given scope are routed through a single `Connection`, we can abstract away the shared `Connection` parameter by wrapping a `ReaderT Connection` around the `IO` monad:

```haskell
newtype PGTransaction a = PGTransaction (ReaderT Connection IO a)
                          deriving (Functor, Applicative, Monad, MonadIO)
```

We can then reimplement our query functions in a more natural fashion:

```haskell
query :: FromRow r => Query -> PGTransaction [a]
execute :: ToRow q => Query -> q -> PGTransaction Int64
```

And we can then use the [postgresql-simple][psqls] `withTransaction` function to provide `runPGTransaction`, which executes a given `PGTransaction` block with rollback semantics:

```haskell
runPGTransaction :: MonadIO m => PGTransaction a -> Postgres.Connection -> m a
```

## About

`postgresql-transactional` was extracted from a production Haskell project at [Helium][helium]. It is open-source software &copy; Helium Systems, Inc., and released to the public under the terms of the MIT license.

[psqls]: https://github.com/lpsmith/postgresql-simple
[helium]: https://www.helium.com
