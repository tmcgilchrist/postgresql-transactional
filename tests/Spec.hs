module Main where

import           Test.Hspec

import qualified Database.PostgreSQL.TransactionSpec as TransactionSpec

main :: IO ()
main = hspec TransactionSpec.spec
