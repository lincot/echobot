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
import qualified Data.HashMap.Lazy             as HML

data SyncState = SyncState
  { next_batch :: Text
  , rooms      :: Rooms
  } deriving (Show, Generic)

instance FromJSON SyncState

newtype Rooms = Rooms
  { joinedRooms :: HML.HashMap Text JoinedRoom
  } deriving (Show, Generic)

instance FromJSON Rooms where
  parseJSON (Object v) = Rooms
    <$> v .: "join"
  parseJSON _          = mzero

newtype JoinedRoom = JoinedRoom
  { timeline :: Timeline
  } deriving (Show, Generic)

instance FromJSON JoinedRoom

data Timeline = Timeline
  { events     :: [RoomEvent]
  , limited    :: Bool
  , prev_batch :: Text
  } deriving (Show, Generic)

instance FromJSON Timeline

data RoomEvent = RoomEvent
  { content   :: Object
  , eventType :: Text
  , event_id  :: Text
  , sender    :: Text
  } deriving (Show, Generic)

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
  } deriving (Show, Generic)

instance ToJSON MessageEvent

data EventResponse
  = NoResponse
  | ResponseSuccess { eventId :: Text }
  | ResponseFailure { errcode :: Text, responseError :: Text }
  deriving (Show, Generic)

instance FromJSON EventResponse where
  parseJSON (Object o) = if HML.member "event_id" o
    then ResponseSuccess <$> o .: "event_id"
    else ResponseFailure <$> o .: "errcode" <*> o .: "error"
  parseJSON _ = pure NoResponse
