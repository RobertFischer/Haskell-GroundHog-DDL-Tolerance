module Main where

import Lib

main :: IO ()
main = do
  dropDb
  initialMigration
  createTriggers
  secondMigration
  createOIDColumns
  finalMigration
