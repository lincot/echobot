{-# OPTIONS -Wno-orphans   #-}
{-# LANGUAGE TupleSections #-}

module Echobot.Bots.Xmpp
  ( xmppBot
  )
where

import           Colog                          ( pattern D
                                                , pattern E
                                                , log
                                                )
import           Network.Xmpp.IM                ( getIM
                                                , withIM
                                                , instantMessage
                                                , InstantMessage(..)
                                                , MessageBody(..)
                                                )
import           Network.Xmpp                   ( Jid
                                                , getMessage
                                                , jidToText
                                                , message
                                                , Message(..)
                                                )
import qualified Network.Xmpp                  as Xmpp
                                                ( sendMessage )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Core.Bot               ( Bot(..) )
import           Echobot.Core.Xmpp              ( Xmpp(..) )

instance ToText Jid where
  toText = jidToText
  {-# INLINE toText #-}

instance Hashable Jid where
  hashWithSalt salt j = hashWithSalt salt . toText $ j
  {-# INLINE hashWithSalt #-}

xmppBot :: App (Bot Text Jid)
xmppBot = Bot getMessagesXmpp sendMessageXmpp pass "XMPP" <$> newIORef mempty

getMessagesXmpp :: App [(Text, Jid, Text)]
getMessagesXmpp = do
  xmpp <- grab
  message'  <- liftIO $ getMessage (xmppSession xmpp)
  case messageFrom message' of
    Just sender -> case getIM message' of
      Just im -> pure $ ("", sender, ) . bodyContent <$> imBody im
      Nothing -> do
        log D "[XMPP] received message with no IM data"
        getMessagesXmpp
    Nothing -> do
      log D "[XMPP] received message with no sender"
      getMessagesXmpp

sendMessageXmpp :: Text -> Text -> App ()
sendMessageXmpp _ msg = do
  xmpp <- grab
  let message' = withIM message { messageTo = Nothing }
                        instantMessage { imBody = [MessageBody Nothing msg] }
  me <- liftIO $ Xmpp.sendMessage message' (xmppSession xmpp)
  case me of
    Left e -> log E $ "[XMPP] " <> show e
    _      -> pass
