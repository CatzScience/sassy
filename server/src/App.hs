{-# LANGUAGE TypeOperators #-}

module App where

import Control.Monad.Logger (runStderrLoggingT)
import Data.String.Conversions

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
        getUsers = error "TODO: implement"
        createUser :: User -> Handler NoContent
        createUser usr = error "TODO: implement"
        getUser :: Username -> Handler User
        getUser uname = error "TODO: implement"
        updateUser :: Username -> User -> Handler User
        updateUser uname usr = error "TODO: implement"
        deleteUser :: Username -> Handler NoContent
        deleteUser uname = error "TODO: implement"

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
