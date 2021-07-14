module Echobot.Bots.Matrix
  ( matrixBot
  ) where

import           Control.Exception              ( throwIO )
import           Data.Aeson                     ( parseJSON )
import           Data.Aeson.Types               ( parseEither )
import qualified Data.Text                     as T
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Bots.Matrix.Types      ( Content(..)
                                                , EventResponse(..)
                                                , JoinedRoom(..)
                                                , RoomEvent(..)
                                                , Rooms(..)
                                                , SyncState(..)
                                                , Timeline(..)
                                                )
import           Echobot.Log                    ( log )
import           Echobot.Types.Bot              ( Bot(..) )
import           Echobot.Types.Matrix           ( Matrix(..) )
import           Echobot.Types.Severity         ( Severity(..) )
import           Network.HTTP.Req
import           UnliftIO.Async                 ( concurrently_ )
import           UnliftIO.Concurrent            ( threadDelay )

matrixBot :: App (Bot (Text, Text) Text)
matrixBot = Bot getMessagesM sendMessageM pass "Matrix" <$> newIORef mempty

instance MonadHttp IO where
  handleHttpException = throwIO
  {-# INLINE handleHttpException #-}

sync :: App (Maybe Rooms)
sync = do
  Matrix {..} <- grab
  since       <- readIORef maSinceR
  let url = apiBase maHomeserver /: "sync"
      limit = "{\"room\":{\"timeline\":{\"limit\":5}}}" :: Text
      params
        =  "access_token" =:    maToken
        <> "filter"       =:    limit
        <> "timeout"      =:    (10000 :: Int)
        <> "since" `queryParam` since
  rb <- responseBody <$> req GET url NoReqBody jsonResponse params
  log D "Matrix" $ "got\n" <> show rb
  writeIORef maSinceR $ Just $ next_batch rb
  pure $ rooms rb

getMessagesM :: App [((Text, Text), Text, Text)]
getMessagesM = do
  Matrix {..} <- grab
  mrooms      <- sync
  case mrooms of
    Just (Rooms (Just hm)) -> do
      let notMyTextMessage = (\RoomEvent{..}
            -> eventType            == "m.room.message"
            && sender               /= maName
            && msgtype content      == "m.text")
          events'' = [ (roomId, filter notMyTextMessage events')
                     | (roomId, JoinedRoom (Just (Timeline (Just events'))))
                     <- toPairs hm ]
          transform = (>>= (\(roomId, events') -> events' <&> (\RoomEvent{..}
                    -> ((roomId, event_id), sender, body content))))
      pure $ transform events''
    _ -> getMessagesM

sendMessageM :: (Text, Text) -> Text -> App ()
sendMessageM (roomId, msgId) msg = do
  Matrix {..} <- grab
  let url = apiBase maHomeserver /: "rooms" /: roomId
          /: "send" /: "m.room.message" /: msgId
      reqBody = ReqBodyJson $ MessageContent "m.text" msg
      params  = "access_token" =: maToken
  rb <- responseBody <$> req PUT url reqBody jsonResponse params
  log D "Matrix" $ "got\n" <> show rb
  case parseEither parseJSON rb of
    Right ResponseSuccess{}          -> pass
    Right (ResponseFailure e me mms) -> again e me mms
    Right NoResponse                 -> again "" Nothing Nothing
    Left  e                          -> again (toText e) Nothing Nothing
 where
  again e me mms = do
    log E "Matrix" $ "could not deliver message:" <> e
      <> maybe "" (T.cons '\n') me
    case mms of
      Nothing -> pass
      Just ms -> concurrently_ (threadDelay $ 1000 * ms)
                               (log W "Matrix" $ "waiting " <> show ms <> " ms")
    sendMessageM (roomId, msgId) msg

apiBase :: Text -> Url 'Https
apiBase homeserver = https homeserver /: "_matrix" /: "client" /: "r0"
