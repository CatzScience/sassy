{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Data.Aeson
import Data.Text
import GHC.Generics

import Database.Persist.TH

type ChallengeId = Int
type Username = Text
type Code = Text

data Language = C | CPlusPlus | Haskell deriving (Show, Eq, Generic)

instance FromJSON Language

data Challenge
  = Challenge {
    challengeId :: ChallengeId,
    title :: Text,
    description :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Challenge

data Submission
  = Submission {
    user :: Username,
    language :: Language,
    body :: Code
  }
  deriving (Show, Eq, Generic)

instance FromJSON Submission

type TestResult = Bool -- TODO: integrate with untest

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  username Username
  realname Text
  email Text
  UniqueUsername username
  deriving Show Eq
|]
