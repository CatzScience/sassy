{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Monad 
import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Maybe
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Database.Persist.Sqlite
import Network.Wai.Handler.Warp as Warp
import Servant

import Api
import Models

server :: ConnectionPool -> Server Api
server pool =
  getChallenges :<|> submitChallenge :<|> getChallenge :<|>
  getUsers :<|> createUser :<|> getUser :<|> updateUser :<|> deleteUser
  where getChallenges :: Handler [Challenge]
        getChallenges = error "TODO: implement"
        getChallenge :: ChallengeId -> Handler Challenge
        getChallenge cid = error "TODO: implement"
        submitChallenge :: Submission -> Handler TestResult
        submitChallenge sub = error "TODO: implement"
        getUsers :: Handler [User]
        getUsers = liftIO . flip runSqlPersistMPool pool $ do
          users <- selectList ([] :: [Filter User]) []
          pure $ entityVal <$> users
        createUser :: User -> Handler (Headers '[Header "Location" T.Text] User)
        createUser usr = mapExceptT (`runSqlPersistMPool` pool) $ do
          exists <- lift . getBy . UniqueUsername $ userUsername usr
          when (isJust exists) $ throwE err409
            {errBody = "User "
              <> TLE.encodeUtf8 (TL.fromStrict $ userUsername usr)
              <> " exists"}
          userId <- lift $ insert usr
          pure $ addHeader ("/users/" <> userUsername usr) usr
        getUser :: Username -> Handler (Maybe User)
        getUser uname = liftIO . flip runSqlPersistMPool pool $ do
          usr <- getBy $ UniqueUsername uname
          pure $ entityVal <$> usr
        updateUser :: Username -> User -> Handler (Maybe User)
        updateUser uname newusr = liftIO . flip runSqlPersistMPool pool $ do
          usr <- getBy $ UniqueUsername uname
          case usr of
            Nothing -> pure Nothing
            Just x -> replace (entityKey x) newusr >> pure (pure newusr)
        deleteUser :: Username -> Handler NoContent
        deleteUser uname = liftIO . flip runSqlPersistMPool pool $ do
          deleteBy $ UniqueUsername uname
          pure NoContent

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ -- do
    createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile
