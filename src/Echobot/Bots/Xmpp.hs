module Echobot.Bots.Xmpp
  ( xmppBot
  , xmppConnect
  ) where

import           Data.Default                   ( def )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Log                    ( log )
import           Echobot.Types.Bot              ( Bot(..) )
import           Echobot.Types.Severity         ( Severity(..) )
import           Network.Xmpp                   ( Jid
                                                , Message(..)
                                                , Session
                                                , getMessage
                                                , jidToText
                                                , message
                                                , scramSha1
                                                , session
                                                )
import qualified Network.Xmpp                  as Xmpp
                                                ( sendMessage )
import           Network.Xmpp.IM                ( InstantMessage(..)
                                                , MessageBody(..)
                                                , getIM
                                                , instantMessage
                                                , withIM
                                                )

instance ToText Jid where
  toText = jidToText
  {-# INLINE toText #-}

instance Hashable Jid where
  hashWithSalt salt = hashWithSalt salt . toText
  {-# INLINE hashWithSalt #-}

xmppConnect :: String -> Text -> Text -> IO Session
xmppConnect host nick pswd = do
  ees <- session host (Just (const [scramSha1 nick Nothing pswd], Nothing)) def
  case ees of
    Right s -> pure s
    Left  e -> error $ "[XMPP] " <> show e

xmppBot :: App (Bot () Jid)
xmppBot =
  Bot getMessagesXmpp (const sendMessageXmpp) pass "XMPP" <$> newIORef mempty

getMessagesXmpp :: App [((), Jid, Text)]
getMessagesXmpp = do
  xmpp     <- grab
  message' <- liftIO $ getMessage xmpp
  case messageFrom message' of
    Just sender -> case getIM message' of
      Just im -> pure $ ((), sender, ) . bodyContent <$> imBody im
      Nothing -> do
        log D "XMPP" "received message with no IM data"
        getMessagesXmpp
    Nothing -> do
      log D "XMPP" "received message with no sender"
      getMessagesXmpp

sendMessageXmpp :: Text -> App ()
sendMessageXmpp msg = do
  xmpp <- grab
  let message' = withIM message { messageTo = Nothing }
                        instantMessage { imBody = [MessageBody Nothing msg] }
  me <- liftIO $ Xmpp.sendMessage message' xmpp
  case me of
    Left e -> log E "XMPP" $ show e
    _      -> pass
