{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Config where

import Control.Monad.Except
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import Data.String.Conversions

import Database.Persist.Sqlite
import Servant

import Models

newtype App a = App
  {
    runApp :: ReaderT Config Handler a
  } deriving (Functor, Applicative, Monad,
              MonadReader Config, MonadError ServantErr, MonadIO)

data Config = Config
  {
    getPool :: ConnectionPool
  }

mkConfig :: FilePath -> IO Config
mkConfig sqliteFile = do
  pool <- runStderrLoggingT $ -- do
    createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ Config pool
