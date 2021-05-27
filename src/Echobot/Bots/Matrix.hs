module Echobot.Bots.Matrix
  ( mBot
  )
where

import           Colog                          ( pattern D
                                                , pattern W
                                                , log
                                                )
import           Data.Aeson                     ( parseJSON )
import           Data.Aeson.Types               ( parseEither
                                                , Value(String)
                                                )
import qualified Data.HashMap.Lazy             as HML
import           Echobot.App.Monad              ( App )
import           Echobot.App.Env                ( grab )
import           Echobot.Bots.Matrix.Types      ( SyncState(..)
                                                , Rooms(..)
                                                , JoinedRoom(..)
                                                , Timeline(..)
                                                , RoomEvent(..)
                                                , MessageEvent(..)
                                                , EventResponse(..)
                                                )
import           Echobot.Core.Bot               ( Bot(..) )
import           Echobot.Core.Matrix            ( Matrix(..) )
import           Network.HTTP.Req

mBot :: App (Bot (Text, Text) Text)
mBot = Bot pass pass getMessagesM sendMessageM "Matrix"
  <$> newIORef mempty

sync :: App Rooms
sync = do
  matrix <- grab
  since' <- readIORef $ mSince matrix
  let url = apiBase (mHomeserver matrix) /: "sync"
      params
        =  "access_token" =:    mToken matrix
        <> "timeout"      =:    (10000 :: Int)
        <> "since" `queryParam` since'
  rb <- responseBody <$> req GET url NoReqBody jsonResponse params
  log D $ "[Matrix] got:\n" <> show rb
  writeIORef (mSince matrix) $ Just $ next_batch rb
  return $ rooms rb

getMessagesM :: App [((Text, Text), Text, Text)]
getMessagesM = do
  matrix <- grab
  rooms' <- sync
  let a :: HML.HashMap Text [RoomEvent]
      a = filter (\event
        -> eventType event == "m.room.message"
        && sender event /= mName matrix
        && HML.lookup "msgtype" (content event) == Just "m.text"
                           ) . events . timeline <$> joinedRooms rooms'
      b :: [((Text, Text), Text, Maybe Value)]
      b = concat $ (\(roomId, events') -> (\event -> (
       (roomId, event_id event), sender event, HML.lookup "body" (content event)
                                                     )) <$> events'
                   ) <$> HML.toList a
      f ((roomId, eId), sender', Just (String msg)) =
        Just ((roomId, eId), sender', msg)
      f _ = Nothing
  return $ catMaybes $ f <$> b

sendMessageM :: (Text, Text) -> Text -> App ()
sendMessageM (roomId, msgId) msg = do
  matrix <- grab
  let url = apiBase (mHomeserver matrix) /: "rooms" /: roomId /: "send"
          /: "m.room.message" /: msgId
      reqBody = ReqBodyJson $ MessageEvent "m.text" msg
      params  = "access_token" =: mToken matrix
  rb <- responseBody <$> req PUT url reqBody jsonResponse params
  log D $ "[Matrix] got:\n" <> show rb
  case parseEither parseJSON rb of
    Right (ResponseSuccess _  ) -> pass
    Right (ResponseFailure _ e) -> again e
    Right NoResponse            -> again ""
    Left  e                     -> again $ toText e
 where
  again e = do
    log W $ "[Matrix] could not deliver message\n" <> e
    sendMessageM (roomId, msgId) msg

apiBase :: Text -> Url 'Https
apiBase homeserver = https homeserver /: "_matrix" /: "client" /: "r0"
