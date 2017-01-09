{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.PostgreSQL.TransactionSpec where

import           Control.Monad.Catch
import           Control.Monad.Except
import qualified Data.ByteString.Char8            as BSC
import           Data.String
import           Data.Typeable
import           Database.PostgreSQL.Simple       (Connection, Only (..),
                                                   SqlError (..))
import qualified Database.PostgreSQL.Simple       as PS
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Transaction
import           System.Process
import           Test.Hspec

-------------------------       Test DB Creation       -------------------------
createDB :: IO Connection
createDB = do
    connectionString <- readProcess "pg_tmp" [] []
    connection <- PS.connectPostgreSQL $ BSC.pack connectionString
    void $ PS.execute_ connection $ fromString
        [sql| CREATE TABLE fruit (name VARCHAR(100) PRIMARY KEY ) |]
    return connection

-------------------------        Test Utilities        -------------------------
insertFruit :: MonadIO m => String -> PGTransactionT m ()
insertFruit fruit
    = void
    $ execute (Only fruit)
    $ fromString [sql| INSERT INTO fruit (name) VALUES (?) |]

fruits :: Connection -> IO [String]
fruits conn
    = fmap (map fromOnly)
    $ PS.query_ conn
    $ fromString [sql|SELECT name FROM fruit ORDER BY name|]

runDB :: Connection -> PGTransaction a -> IO a
runDB = flip runPGTransactionIO

-- Simple exception type for testing
data Forbidden = Forbidden
    deriving (Show, Eq, Typeable)

instance Exception Forbidden

-------------------------         Tests Start          -------------------------
spec :: Spec
spec = describe "TransactionSpec" $ do
    -- Notice the 'beforeAll'. The second test uses the same db as the first
    beforeAll createDB $ do
        it "execute_ happen path succeeds" $ \conn -> do
            let apple = "apple"
            runDB conn $ insertFruit apple

            fruits conn `shouldReturn` ["apple"]

        it "execute_ rollbacks on exception" $ \conn -> do
            flip shouldThrow (\(SqlError {}) -> True) $
                runDB conn $ do
                    insertFruit "orange"
                    -- This should cause an exception because of the UNIQUE
                    -- constraint on 'name'
                    insertFruit "apple"

            fruits conn `shouldReturn` ["apple"]

    before createDB $ do
        it "multiple execute_'s succeed" $ \conn -> do
            runDB conn $ do
                insertFruit "grapes"
                insertFruit "orange"

            fruits conn `shouldReturn` ["grapes", "orange"]

        it "throwM causes a rollback" $ \conn -> do
            flip shouldThrow (\Forbidden -> True) $
                runDB conn $ do
                    insertFruit "salak"
                    () <- throwM Forbidden
                    insertFruit "banana"

            fruits conn `shouldReturn` []

        it "throwError causes a rollback" $ \conn -> do
            e <- runExceptT $ flip runPGTransactionT conn $ do
                    insertFruit "salak"
                    () <- throwError Forbidden
                    insertFruit "banana"

            e `shouldBe` Left Forbidden

        it "query recovers when exception is caught" $ \conn -> do
            runDB conn $ do
                -- This should always happen because of the handle below
                insertFruit "banana"
                handle (\Forbidden -> insertFruit "tomato") $ do
                    insertFruit "salak"
                    throwM Forbidden

            fruits conn `shouldReturn` ["banana", "tomato"]

        it "multiple catch statements work correctly" $ \conn -> do
            runDB conn $ do
                insertFruit "banana"
                handle (\Forbidden -> insertFruit "tomato") $ do
                    -- This will happen ... even if there is an exception below
                    -- if we catch it
                    insertFruit "blueberry"
                    handle (\Forbidden -> insertFruit "frankenberry") $ do
                        insertFruit "salak"
                        throwM Forbidden

            fruits conn `shouldReturn` ["banana", "blueberry", "frankenberry"]

        it "alternate branches can also have savepoints" $ \conn -> do
            runDB conn $ do
                insertFruit "banana"
                catch (insertFruit "tomato" >> throwM Forbidden) $
                    \Forbidden -> do
                        insertFruit "blueberry"
                        handle (\Forbidden -> insertFruit "frankenberry") $ do
                            insertFruit "salak"
                            throwM Forbidden

            fruits conn `shouldReturn` ["banana", "blueberry", "frankenberry"]

        it "releasing silently fails if the transaction errors" $ \conn -> do
            runDB conn $ do
                insertFruit "banana"
                catchAll (void $ execute_ $ fromString [sql| ABORT |]) $
                    \_ -> insertFruit "tomato"

            fruits conn `shouldReturn` []
