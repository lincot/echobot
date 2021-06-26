{-# LANGUAGE FlexibleContexts #-}

module Echobot.Runner
  ( botRunner
  )
where

import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Db                     ( getUser
                                                , putUser
                                                )
import           Echobot.Log                    ( log )
import           Echobot.Types.Severity         ( Severity(..) )
import           Echobot.Types.Bot              ( Bot(..) )
import           Echobot.Types.Dflts            ( Dflts(..) )
import           Echobot.Types.Msgs             ( Msgs(..) )
import           Echobot.Types.Users            ( BotMode(..)
                                                , User(..)
                                                )
import           UnliftIO.Exception             ( onException )

botRunner :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> App ()
botRunner bot@Bot {..} = do
  log I botName "bot started"
  onException (runBot bot) disableBot

runBot :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> App ()
runBot bot@Bot {..} = forever $ do
  msgs <- getMessages
  mapM
    (\(chan, uid, msg) -> do
      log I botName $ "received message from " <> toText uid <> "\n" <> msg
      react bot chan uid msg
    )
    msgs

react :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> c -> u -> Text -> App ()
react bot@Bot {..} chan uid msg = do
  musr <- getUser users uid
  case musr of
    Just usr -> case userMode usr of
      AwaitingRepeatCountMode -> reactRepeatCount bot chan uid usr msg
      NormalMode              -> reactNormal      bot chan uid usr msg
    Nothing -> do
      Dflts {..} <- grab
      let usr =
            User { userMode = NormalMode, userRepeatCount = defaultRepeatCount }
      putUser users uid usr
      reactNormal bot chan uid usr msg

reactNormal :: (Eq u, Hashable u) =>
  Bot c u -> c -> u -> User -> Text -> App ()
reactNormal bot@Bot {..} chan uid usr "/repeat" = do
  Msgs {..} <- grab
  sendMessage' bot chan $ repeat1Msg <> show (userRepeatCount usr) <> repeat2Msg
  putUser users uid usr { userMode = AwaitingRepeatCountMode }
reactNormal bot chan _ _ msg | msg == "/help" || msg == "/start" = do
  Msgs {..} <- grab
  sendMessage' bot chan helpMsg
reactNormal bot chan _ usr msg =
  replicateM_ (userRepeatCount usr) $ sendMessage' bot chan msg

reactRepeatCount :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> c -> u -> User -> Text -> App ()
reactRepeatCount bot@Bot {..} chan uid usr msg =
  case readMaybe $ toString msg of
    Just c -> if
      | c < 0 -> invalid "a negative"
      | 5 < c -> invalid "too big"
      | otherwise -> do
        log I botName $ "changing repeat count to " <> show c
          <> " for " <> toText uid
        putUser users uid usr { userRepeatCount = c, userMode = NormalMode }
    Nothing -> invalid "not a"
 where
  invalid n = do
    log W botName $ "got " <> n <> " number from " <> toText uid
    Msgs {..} <- grab
    sendMessage' bot chan invalidMsg

sendMessage' :: Bot c u -> c -> Text -> App ()
sendMessage' Bot{..} chan msg = do
  log I botName $ "sending message\n" <> msg
  sendMessage chan msg
