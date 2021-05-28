{-# LANGUAGE DeriveAnyClass #-}

module Echobot.Bots.Telegram.Types
  ( TgResponse(..)
  , TgUpdate(..)
  , TgMessage(..)
  , TgChat(..)
  , TgUser(..)
  )
where

import           Data.Aeson

data TgResponse a = TgResponse
  { description :: Maybe Text
  , result      :: Maybe a
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (TgResponse a)

data TgUpdate = TgUpdate
  { update_id :: Int
  , message   :: Maybe TgMessage
  } deriving (Show, Generic, FromJSON)

data TgMessage = TgMessage
  { from :: Maybe TgUser
  , chat :: TgChat
  , text :: Maybe Text
  } deriving (Show, Generic, FromJSON)

newtype TgChat = TgChat
  { chatId :: Int
  } deriving Show

instance FromJSON TgChat where
  parseJSON (Object v) = TgChat
    <$> v .:  "id"
  parseJSON _          = mzero

newtype TgUser = TgUser
  { id :: Int
  } deriving Show

instance FromJSON TgUser where
  parseJSON (Object v) = TgUser
    <$> v .:  "id"
  parseJSON _          = mzero
