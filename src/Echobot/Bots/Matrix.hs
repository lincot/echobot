module Echobot.Bots.Matrix
  ( matrixBot
  )
where

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
import           Echobot.Log                    ( log )
import           Echobot.Types.Severity         ( Severity(..) )
import           Echobot.Types.Bot              ( Bot(..) )
import           Echobot.Types.Matrix           ( Matrix(..) )
import           Network.HTTP.Req

matrixBot :: App (Bot (Text, Text) Text)
matrixBot = Bot getMessagesM sendMessageM pass "Matrix" <$> newIORef mempty

sync :: App Rooms
sync = do
  Matrix {..} <- grab
  since       <- readIORef mSince
  let url = apiBase mHomeserver /: "sync"
      params
        =  "access_token" =:    mToken
        <> "timeout"      =:    (10000 :: Int)
        <> "since" `queryParam` since
  rb <- responseBody <$> req GET url NoReqBody jsonResponse params
  log D "Matrix" $ "got\n" <> show rb
  writeIORef mSince $ Just $ next_batch rb
  pure $ rooms rb

getMessagesM :: App [((Text, Text), Text, Text)]
getMessagesM = do
  Matrix {..} <- grab
  rooms'      <- sync
  let a :: HashMap Text [RoomEvent]
      a = filter (\RoomEvent{..}
        -> eventType == "m.room.message"
        && sender /= mName
        && HML.lookup "msgtype" content == Just "m.text"
                 ) . events . timeline <$> joinedRooms rooms'
      b :: [((Text, Text), Text, Maybe Value)]
      b = HML.toList a >>= (\(roomId, events') -> events' <&> (\RoomEvent{..}
        -> ((roomId, event_id), sender, HML.lookup "body" content)))
  pure [ ((roomId, eId), sender', msg)
       | ((roomId, eId), sender', Just (String msg))
       <- b ]

sendMessageM :: (Text, Text) -> Text -> App ()
sendMessageM (roomId, msgId) msg = do
  Matrix {..} <- grab
  let url = apiBase mHomeserver /: "rooms" /: roomId
          /: "send" /: "m.room.message" /: msgId
      reqBody = ReqBodyJson $ MessageEvent "m.text" msg
      params  = "access_token" =: mToken
  rb <- responseBody <$> req PUT url reqBody jsonResponse params
  log D "Matrix" $ "got\n" <> show rb
  case parseEither parseJSON rb of
    Right ResponseSuccess {}    -> pass
    Right (ResponseFailure _ e) -> again e
    Right NoResponse            -> again ""
    Left  e                     -> again $ toText e
 where
  again e = do
    log W "Matrix" $ "could not deliver message\n" <> e
    sendMessageM (roomId, msgId) msg

apiBase :: Text -> Url 'Https
apiBase homeserver = https homeserver /: "_matrix" /: "client" /: "r0"
