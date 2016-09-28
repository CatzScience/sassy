{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Models

import Data.Proxy
import Data.Text
import Servant.API

type Api =
  "challenges" :> -- get a list of challenges
    Get '[JSON] [Challenge]
  :<|> "challenges" :> -- submit a challenge solution
    ReqBody '[JSON] Submission :> Post '[JSON] TestResult
  :<|> "challenges" :> -- get a specific challenge
    Capture "challengeId" ChallengeId :> Get '[JSON] Challenge
  :<|> "users" :> -- get a list of users
    -- TODO: query params on returned data
    Get '[JSON] [User]
  :<|> "users" :> -- create a user
    ReqBody '[JSON] User :> PostCreated '[JSON] (Headers '[Header "Location" Text] User)
  :<|> "users" :> -- get a specific user
    Capture "username" Username :> Get '[JSON] (Maybe User)
  :<|> "users" :> -- update a specific user
    Capture "username" Username :> ReqBody '[JSON] User :> Put '[JSON] (Maybe User)
  :<|> "users" :> -- delete a specific user
    Capture "username" Username :> DeleteNoContent '[JSON] NoContent

api :: Proxy Api
api = Proxy

