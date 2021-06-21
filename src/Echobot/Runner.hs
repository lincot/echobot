{-# LANGUAGE FlexibleContexts #-}

module Echobot.Runner
  ( botRunner
  )
where

import           Colog                          ( pattern I
                                                , pattern W
                                                , log
                                                , Msg
                                                , WithLog
                                                )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Types.Bot              ( Bot(..) )
import           Echobot.Types.Dflts            ( Dflts(..) )
import           Echobot.Types.Msgs             ( Msgs(..) )
import           Echobot.Types.Users            ( BotMode(..)
                                                , User(..)
                                                )
import           Echobot.Db                     ( getUser
                                                , putUser
                                                )
import           UnliftIO.Exception             ( onException )

botRunner :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> App ()
botRunner bot = do
  log' bot I "bot started"
  onException (runBot bot) $ disableBot bot

runBot :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> App ()
runBot bot = forever $ do
  msgs <- getMessages' bot
  mapM (\(c, u, t) -> react bot c u t) msgs

react :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> c -> u -> Text -> App ()
react bot chan src msg = do
  musr <- getUser (users bot) src
  case musr of
    Just usr -> case userMode usr of
      AwaitingRepeatCountMode -> reactRepeatCount bot chan src usr msg
      NormalMode              -> reactNormal      bot chan src usr msg
    Nothing -> do
      dflts <- grab
      let usr = User { userMode        = NormalMode
                     , userRepeatCount = defaultRepeatCount dflts
                     }
      putUser (users bot) src usr
      reactNormal bot chan src usr msg

reactNormal :: (Eq u, Hashable u) =>
  Bot c u -> c -> u -> User -> Text -> App ()
reactNormal bot chan src usr "/repeat" = do
  msgs <- grab
  sendMessage' bot chan $  repeat1Msg msgs <> show (userRepeatCount usr)
                        <> repeat2Msg msgs
  putUser (users bot) src usr { userMode = AwaitingRepeatCountMode }
reactNormal bot chan _ _ msg | msg == "/help" || msg == "/start" = do
  msgs <- grab
  sendMessage' bot chan $ helpMsg msgs
reactNormal bot chan _ usr msg =
  replicateM_ (userRepeatCount usr) $ sendMessage' bot chan msg

reactRepeatCount :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> c -> u -> User -> Text -> App ()
reactRepeatCount bot chan src usr msg = case readMaybe $ toString msg of
  Just c -> if
    | c < 0 -> invalid "too low"
    | 5 < c -> invalid "too big"
    | otherwise -> do
      log' bot I $ "changing " <> toText src <> "'s repeat count to " <> show c
      putUser (users bot) src usr { userRepeatCount = c, userMode = NormalMode }
  Nothing -> invalid "not a"
 where
  invalid n = do
    log' bot W $ "got " <> n <> " number from " <> toText src
    msgs <- grab
    sendMessage' bot chan $ invalidMsg msgs

sendMessage' :: Bot c u -> c -> Text -> App ()
sendMessage' bot c t = do
  log' bot I $ "sending message:\n" <> t
  sendMessage bot c t

getMessages' :: ToText u => Bot c u -> App [(c, u, Text)]
getMessages' bot = do
  msgs <- getMessages bot
  mapM_
    (\(_, u, t) -> log' bot I $ "received message:\n" <> toText u <> ": " <> t)
    msgs
  pure msgs

log' :: WithLog env (Msg sev) m => Bot c u -> sev -> Text -> m ()
log' bot sev = log sev . (("[" <> botName bot <> "] ") <>)
