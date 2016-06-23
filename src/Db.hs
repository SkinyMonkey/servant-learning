{-# LANGUAGE FlexibleContexts           #-}

module Db where

import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Database.Persist.Sql
import Config

import Models

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

doMigrations :: SqlPersistT IO ()
doMigrations = do 
  runMigration migrateUser
  runMigration migrateTrack
