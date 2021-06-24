{-# LANGUAGE FlexibleContexts #-}

module Echobot.Runner
  ( botRunner
  )
where

import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Log                    ( log )
import           Echobot.Types.Severity         ( Severity(..) )
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
  msgs <- getMessages bot
  mapM
    (\(chan, uid, msg) -> do
      log' bot I $ "received message from " <> toText uid <> "\n" <> msg
      react bot chan uid msg
    )
    msgs

react :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> c -> u -> Text -> App ()
react bot chan uid msg = do
  musr <- getUser (users bot) uid
  case musr of
    Just usr -> case userMode usr of
      AwaitingRepeatCountMode -> reactRepeatCount bot chan uid usr msg
      NormalMode              -> reactNormal      bot chan uid usr msg
    Nothing -> do
      dflts <- grab
      let usr = User { userMode        = NormalMode
                     , userRepeatCount = defaultRepeatCount dflts
                     }
      putUser (users bot) uid usr
      reactNormal bot chan uid usr msg

reactNormal :: (Eq u, Hashable u) =>
  Bot c u -> c -> u -> User -> Text -> App ()
reactNormal bot chan uid usr "/repeat" = do
  msgs <- grab
  sendMessage' bot chan $ repeat1Msg msgs <> show (userRepeatCount usr)
                       <> repeat2Msg msgs
  putUser (users bot) uid usr { userMode = AwaitingRepeatCountMode }
reactNormal bot chan _ _ msg | msg == "/help" || msg == "/start" = do
  msgs <- grab
  sendMessage' bot chan $ helpMsg msgs
reactNormal bot chan _ usr msg =
  replicateM_ (userRepeatCount usr) $ sendMessage' bot chan msg

reactRepeatCount :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> c -> u -> User -> Text -> App ()
reactRepeatCount bot chan uid usr msg = case readMaybe $ toString msg of
  Just c -> if
    | c < 0 -> invalid "a negative"
    | 5 < c -> invalid "too big"
    | otherwise -> do
      log' bot I $ "changing " <> toText uid <> "'s repeat count to " <> show c
      putUser (users bot) uid usr { userRepeatCount = c, userMode = NormalMode }
  Nothing -> invalid "not a"
 where
  invalid n = do
    log' bot W $ "got " <> n <> " number from " <> toText uid
    msgs <- grab
    sendMessage' bot chan $ invalidMsg msgs

sendMessage' :: Bot c u -> c -> Text -> App ()
sendMessage' bot chan msg = do
  log' bot I $ "sending message\n" <> msg
  sendMessage bot chan msg

log' :: Bot c u -> Severity -> Text -> App ()
log' = flip log . botName
