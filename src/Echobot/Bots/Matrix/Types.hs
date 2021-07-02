{-# LANGUAGE DeriveAnyClass #-}

module Echobot.Bots.Matrix.Types
  ( SyncState(..)
  , Rooms(..)
  , JoinedRoom(..)
  , Timeline(..)
  , RoomEvent(..)
  , MessageEvent(..)
  , EventResponse(..)
  )
where

import           Data.Aeson

data SyncState = SyncState
  { next_batch :: Text
  , rooms      :: Maybe Rooms
  } deriving (Show, Generic, FromJSON)

newtype Rooms = Rooms
  { joinedRooms :: Maybe (HashMap Text JoinedRoom)
  } deriving stock Show

instance FromJSON Rooms where
  parseJSON (Object v) = Rooms
    <$> v .:? "join"
  parseJSON _          = mzero

newtype JoinedRoom = JoinedRoom
  { timeline :: Maybe Timeline
  } deriving stock (Show, Generic)
    deriving anyclass FromJSON

newtype Timeline = Timeline
  { events     :: Maybe [RoomEvent]
  } deriving stock (Show, Generic)
    deriving anyclass FromJSON

data RoomEvent = RoomEvent
  { content :: Object
  , eventType, event_id, sender :: Text
  } deriving Show

instance FromJSON RoomEvent where
  parseJSON (Object v) = RoomEvent
    <$> v .: "content"
    <*> v .: "type"
    <*> v .: "event_id"
    <*> v .: "sender"
  parseJSON _ = mzero

data MessageEvent = MessageEvent
  { msgtype :: Text
  , body    :: Text
  } deriving (Show, Generic, ToJSON)

data EventResponse
  = NoResponse
  | ResponseSuccess { eventId :: Text }
  | ResponseFailure { errcode        :: Text
                    , responseError  :: Maybe Text
                    , retry_after_ms :: Maybe Int
                    }
  deriving Show

instance FromJSON EventResponse where
  parseJSON (Object o) = if "event_id" `member` o
    then ResponseSuccess
      <$> o .: "event_id"
    else ResponseFailure
      <$> o .:  "errcode"
      <*> o .:? "error"
      <*> o .:? "retry_after_ms"
  parseJSON _ = pure NoResponse
