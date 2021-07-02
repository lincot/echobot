module Echobot.Bots.Matrix
  ( matrixBot
  )
where

import           Data.Aeson                     ( parseJSON )
import           Data.Aeson.Types               ( parseEither
                                                , Value(String)
                                                )
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
import           UnliftIO.Async                 ( concurrently_ )
import           UnliftIO.Concurrent            ( threadDelay )

matrixBot :: App (Bot (Text, Text) Text)
matrixBot = Bot getMessagesM sendMessageM pass "Matrix" <$> newIORef mempty

sync :: App (Maybe Rooms)
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
  mrooms      <- sync
  case mrooms of
    Just (Rooms (Just hm)) -> do
      let notMyTextMessage = (\RoomEvent{..}
            -> eventType            == "m.room.message"
            && sender               /= mName
            && content !? "msgtype" == Just "m.text")
          events'' = [ (roomId, filter notMyTextMessage events')
                     | (roomId, JoinedRoom (Just (Timeline (Just events'))))
                     <- toPairs hm ]
          transform = (>>= (\(roomId, events') -> events' <&> (\RoomEvent{..}
                    -> ((roomId, event_id), sender, content !? "body"))))
      pure [ ((roomId, eId), sender', msg)
           | ((roomId, eId), sender', Just (String msg))
           <- transform events'' ]
    _ -> getMessagesM

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
    Right ResponseSuccess{}          -> pass
    Right (ResponseFailure e me mms) -> again e me mms
    Right NoResponse                 -> again "" Nothing Nothing
    Left  e                          -> again (toText e) Nothing Nothing
 where
  again e me mms = do
    log E "Matrix" $ "could not deliver message:" <> e <> maybe "" ("\n" <>) me
    case mms of
      Nothing -> pass
      Just ms -> concurrently_ (threadDelay $ 1000 * ms)
                               (log W "Matrix" $ "waiting " <> show ms <> " ms")
    sendMessageM (roomId, msgId) msg

apiBase :: Text -> Url 'Https
apiBase homeserver = https homeserver /: "_matrix" /: "client" /: "r0"
