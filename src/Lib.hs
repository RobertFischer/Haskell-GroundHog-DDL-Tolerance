{-# LANGUAGE
   FlexibleContexts,
   GADTs,
   TypeFamilies,
   TemplateHaskell,
   QuasiQuotes,
   FlexibleInstances,
   StandaloneDeriving,
   OverloadedStrings #-}

module Lib
  (
    dropDb
  , initialMigration
  , secondMigration
  , finalMigration
  , createTriggers
  , createOIDColumns
  ) where

import Prelude hiding (log)
import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Database.Groundhog hiding (withConn)
import Database.Groundhog.Core hiding (withConn)
import Database.Groundhog.TH
import Database.Groundhog.Postgresql
import Database.PostgreSQL.Simple (Connection, execute_)

dbConnStr = ""

log = hPutStrLn stderr

withDb :: (ExtractConnection Postgresql conn, ConnectionManager conn) => Action conn a -> IO a
withDb action = withPostgresqlConn dbConnStr doRun
  where
    doRun = runDbConn action

withConn :: (Connection -> IO a) -> IO a
withConn action = withPostgresqlConn dbConnStr doRun
  where
    doRun (Postgresql conn) = action conn

data Foo = Foo {
  foo :: String
} deriving Show

data Bar = Bar {
  bar :: String
} deriving Show

data Baz = Baz {
  baz :: String
} deriving Show

mkPersist defaultCodegenConfig [groundhog|
- entity: Foo
- entity: Bar
- entity: Baz
|]


dropDb :: IO ()
dropDb = withConn $ \conn -> do
  log "Dropping Foo"
  _ <- execute_ conn "DROP TABLE IF EXISTS \"Foo\""
  log "Dropping Bar"
  _ <- execute_ conn "DROP TABLE IF EXISTS \"Bar\""
  log "Dropping Baz"
  _ <- execute_ conn "DROP TABLE IF EXISTS \"Baz\""
  log "Dropping triggerWarning"
  _ <- execute_ conn "DROP TRIGGER IF EXISTS triggerWarning ON \"Foo\""
  log "Dropping triggerWarningProc"
  _ <- execute_ conn "DROP FUNCTION IF EXISTS triggerWarningProc()"
  log "Done dropping the DB"
  return ()

initialMigration :: IO ()
initialMigration = do
  log "Running initial migration"
  withDb $ runMigration $ do
    migrate (undefined :: Foo)
  log "Done with initial migration"

createTriggers :: IO ()
createTriggers = withConn $ \conn -> do
  log "Creating triggerWarningProc"
  _ <- execute_ conn "CREATE OR REPLACE FUNCTION triggerWarningProc() RETURNS trigger AS $$ BEGIN RAISE WARNING 'This is a warning. There are many like it, but this one is mine.'; END $$ LANGUAGE plpgsql"
  log "Creating triggerWarning"
  _ <- execute_ conn "CREATE TRIGGER triggerWarning AFTER INSERT ON \"Foo\" EXECUTE PROCEDURE triggerWarningProc()"
  log "Done creating triggers"
  return ()

secondMigration :: IO ()
secondMigration = do
  log "Running second migration"
  withDb $ runMigration $ do
      migrate (undefined :: Foo)
      migrate (undefined :: Bar)
  log "Done with second migration"

createOIDColumns :: IO ()
createOIDColumns = withConn $ \conn -> do
  log "Attaching OIDs to Foo"
  _ <- execute_ conn "ALTER TABLE \"Foo\" SET WITH OIDS"
  log "Done attaching OIDs to Foo"
  return ()

finalMigration :: IO ()
finalMigration = do
  log "Running final migration"
  withDb $ runMigration $ do
    migrate (undefined :: Foo)
    migrate (undefined :: Bar)
    migrate (undefined :: Baz)
  log "Done with final migration"
