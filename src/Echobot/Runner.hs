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
import           Echobot.Core.Bot               ( Bot(..) )
import           Echobot.Core.Dflts             ( Dflts(..) )
import           Echobot.Core.Msgs              ( Msgs(..) )
import           Echobot.Core.Users             ( BotMode(..)
                                                , User(..)
                                                )
import           Echobot.Db                     ( getUser
                                                , putUser
                                                )
import           Text.Printf                    ( printf )
import           UnliftIO.Exception             ( finally )

botRunner :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> App ()
botRunner bot = do
  log' bot I "bot started"
  finally (startBot bot >> runBot bot) $ disableBot bot

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
      let
        usr = User { userMode        = NormalMode
                   , userRepeatCount = defaultRepeatCount dflts
                   }
      putUser (users bot) src usr
      reactNormal bot chan src usr msg

reactNormal :: (Eq u, Hashable u) =>
  Bot c u -> c -> u -> User -> Text -> App ()
reactNormal bot chan src usr "/repeat" = do
  msgs <- grab
  sendMessage' bot chan $ toText
    (printf (toString $ repeatMsg msgs) (userRepeatCount usr) :: String)
  putUser (users bot) src usr { userMode = AwaitingRepeatCountMode }
reactNormal bot chan _ _ msg | msg == "/help" || msg == "/start" = do
  msgs <- grab
  sendMessage' bot chan $ helpMsg msgs
reactNormal bot chan _ usr msg =
  replicateM_ (userRepeatCount usr) $ sendMessage' bot chan msg

reactRepeatCount :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> c -> u -> User -> Text -> App ()
reactRepeatCount bot chan src usr msg = case readMaybe $ toString msg of
  Just c -> if c < 0 || 5 < c
    then do
      log' bot W $ "got too low/too big number from " <> toText src
      again
    else do
      log' bot I $ "changing " <> toText src <> "'s repeat count to " <> show c
      putUser (users bot) src usr { userRepeatCount = c, userMode = NormalMode }
  Nothing -> do
    log' bot W $ "got not a number from " <> toText src
    again
 where
  again = do
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
