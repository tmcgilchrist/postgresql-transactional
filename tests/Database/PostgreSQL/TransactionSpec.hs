{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Database.PostgreSQL.TransactionSpec where

import           Control.Monad              (void)
import           Control.Monad.Catch
import qualified Data.ByteString.Char8      as BSC
import           Data.String
import           Data.Typeable
import qualified Database.PostgreSQL.Simple as PS
import           Database.PostgreSQL.Simple ( Connection
                                            , Only (..)
                                            , SqlError (..)
                                            )
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Transaction
import           System.Process
import           Test.Hspec

data Forbidden = Forbidden
    deriving (Show, Eq, Typeable)

instance Exception Forbidden

createDB :: IO Connection
createDB = do
    connectionString <- readProcess "pg_tmp" [] []
    connection <- PS.connectPostgreSQL $ BSC.pack connectionString
    void $ PS.execute_ connection $ fromString
        [sql| CREATE TABLE fruit (name VARCHAR(100) PRIMARY KEY ) |]
    return connection

insertFruit :: String -> PGTransactionT IO ()
insertFruit fruit
    = void
    $ execute (Only fruit)
    $ fromString [sql| INSERT INTO fruit (name) VALUES (?) |]

fruitAlphaOrdered :: Connection -> IO [String]
fruitAlphaOrdered conn
    = fmap (map fromOnly)
    $ PS.query_ conn
    $ fromString [sql|SELECT name FROM fruit ORDER BY name|]

runDB :: Connection -> PGTransaction a -> IO a
runDB = flip runPGTransactionIO

spec :: Spec
spec = describe "TransactionSpec" $ do
    beforeAll createDB $ do
        it "execute_ happen path succeeds" $ \conn -> do
            let apple = "apple"
            runDB conn $ insertFruit apple

            fruitRows <- fruitAlphaOrdered conn
            fruitRows `shouldBe` ["apple"]

        it "execute_ rollbacks on exception" $ \conn -> do
            flip shouldThrow (\(SqlError {}) -> True) $
                runDB conn $ do
                    insertFruit "orange"
                    -- This should cause an exception because of the UNIQUE
                    -- constraint on 'name'
                    insertFruit "apple"

            fruitRows <- fruitAlphaOrdered conn
            fruitRows `shouldBe` ["apple"]
    before createDB $ do
        it "multiple execute_'s succeed" $ \conn -> do
            runDB conn $ do
                insertFruit "grapes"
                insertFruit "orange"

            fruitRows <- fruitAlphaOrdered conn
            fruitRows `shouldBe` ["grapes", "orange"]

        it "throwM causes a rollback" $ \conn -> do
            flip shouldThrow (\Forbidden -> True) $
                runDB conn $ do
                    insertFruit "salak"
                    () <- throwM Forbidden
                    insertFruit "banana"

            fruitRows <- fruitAlphaOrdered conn
            fruitRows `shouldBe` []

        it "query recovers when exception is caught" $ \conn -> do
            runDB conn $ do
                -- This should always happen
                insertFruit "banana"
                handle (\Forbidden -> insertFruit "tomato") $ do
                    insertFruit "salak"
                    throwM Forbidden


            fruitRows <- fruitAlphaOrdered conn
            fruitRows `shouldBe` ["banana", "tomato"]

        it "multiple catch statements work correctly" $ \conn -> do
            runDB conn $ do
                -- This should always happen
                insertFruit "banana"
                handle (\Forbidden -> insertFruit "tomato") $ do
                    -- This will happen ... even if there is an exception below
                    -- if we catch it
                    insertFruit "blueberry"
                    handle (\Forbidden -> insertFruit "frankenberry") $ do
                        insertFruit "salak"
                        throwM Forbidden

            fruitRows <- fruitAlphaOrdered conn
            fruitRows `shouldBe` ["banana", "blueberry", "frankenberry"]

        it "releasing silently fails if the transaction errors" $ \conn -> do
            runDB conn $ do
                insertFruit "banana"
                catchAll (void $ execute_ $ fromString [sql| ABORT |]) $ 
                    \_ -> insertFruit "tomato"

            fruitRows <- fruitAlphaOrdered conn
            fruitRows `shouldBe` []
