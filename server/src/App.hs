{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Monad 
import Control.Monad.Reader
import Data.Maybe
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Database.Persist.Sqlite
import Network.Wai.Handler.Warp as Warp
import Servant

import Api
import Config
import Models

server :: ServerT Api App
server =
  getChallenges :<|> submitChallenge :<|> getChallenge :<|>
  getUsers :<|> createUser :<|> getUser :<|> updateUser :<|> deleteUser
  where getChallenges :: App [Challenge]
        getChallenges = error "TODO: implement"
        getChallenge :: ChallengeId -> App Challenge
        getChallenge cid = error "TODO: implement"
        submitChallenge :: Submission -> App TestResult
        submitChallenge sub = error "TODO: implement"
        getUsers :: App [User]
        getUsers = do
          users <- runDb $ selectList ([] :: [Filter User]) []
          pure $ entityVal <$> users
        createUser :: User -> App (Headers '[Header "Location" T.Text] User)
        createUser usr = do
          exists <- runDb $ getBy (UniqueUsername $ userUsername usr)
          when (isJust exists) $ throwError err409
            {errBody = "User "
              <> TLE.encodeUtf8 (TL.fromStrict $ userUsername usr)
              <> " exists"}
          userId <- runDb $ insert usr
          pure $ addHeader ("/users/" <> userUsername usr) usr
        getUser :: Username -> App (Maybe User)
        getUser uname = do
          usr <- runDb $ getBy (UniqueUsername uname)
          pure $ entityVal <$> usr
        updateUser :: Username -> User -> App (Maybe User)
        updateUser uname newusr = do
          usr <- runDb $ getBy (UniqueUsername uname)
          case usr of
            Nothing -> pure Nothing
            Just x -> runDb (replace (entityKey x) newusr) >> pure (pure newusr)
        deleteUser :: Username -> App NoContent
        deleteUser uname = do
          runDb $ deleteBy (UniqueUsername uname)
          pure NoContent

app :: Config -> Application
app cfg = serve api $ withConfig cfg

withConfig :: Config -> Server Api
withConfig cfg = enter (convertApp cfg) server

convertApp :: Config -> App :~> Handler
convertApp cfg = Nat (flip runReaderT cfg . runApp)

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO a -> m a
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< app <$> mkConfig sqliteFile
